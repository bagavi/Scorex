package examples.prism1.mining

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import examples.commons.SimpleBoxTransactionPrismMemPool
import examples.prism1.blocks.{HybridBlock, PowBlock, PowBlockCompanion, PowBlockHeader}
import examples.prism1.history.HybridHistory
import examples.prism1.state.HBoxStoredState
import examples.prism1.util.Cancellable
import examples.prism1.wallet.HBoxWallet
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.block.Block.BlockId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexEncoding
import scorex.crypto.hash.Blake2b256
import scorex.util.{ModifierId, ScorexLogging}

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Random

/**
  * A controller for PoW mining
  * currently it is starting to work on getting a (PoW; PoS) block references
  * and stops on a new PoW block found (when PoS ref is unknown)
  */
class PowMiner(viewHolderRef: ActorRef, settings: HybridMiningSettings)(implicit ec: ExecutionContext)
  extends Actor with ScorexLogging with ScorexEncoding {

  import PowMiner.ReceivableMessages._
  import PowMiner._
  import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}


  private var cancellableOpt: Option[Cancellable] = None
  private var mining = false
  private val getRequiredData: GetDataFromCurrentView[HybridHistory,
    HBoxStoredState,
    HBoxWallet,
    SimpleBoxTransactionPrismMemPool,
    PowMiningInfo] = {
    val f: CurrentView[HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionPrismMemPool] => PowMiningInfo = {
      view: CurrentView[HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionPrismMemPool] =>

        val difficulty = view.history.powDifficulty
        val pairCompleted = view.history.pairCompleted
        val bestPowBlock = view.history.bestPowBlock
        val bestPosId = view.history.bestPosId
        // TODO: fixme, What should we do if `view.vault.generateNewSecret().publicKeys` is empty?
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val pubkey = view.vault.publicKeys.headOption getOrElse view.vault.generateNewSecret().publicKeys.head
        PowMiningInfo(pairCompleted, difficulty, bestPowBlock, bestPosId, pubkey)
    }
    GetDataFromCurrentView[HybridHistory,
      HBoxStoredState,
      HBoxWallet,
      SimpleBoxTransactionPrismMemPool,
      PowMiningInfo](f)
  }


  override def preStart(): Unit = {
    //todo: check for a last block (for what?)
    if (settings.offlineGeneration) {
      context.system.scheduler.scheduleOnce(1.second)(self ! StartMining)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Serializable"))
  override def receive: Receive = {
    case StartMining =>
      if (settings.blockGenerationDelay >= 1.minute) {
        log.info("Mining is disabled for blockGenerationDelay >= 1 minute")
      } else {
        mining = true
        self ! MineBlock
      }

    case MineBlock =>
      if (mining) {
        log.info("Mining of previous PoW block stopped")
        cancellableOpt.forall(_.cancel())

        context.system.scheduler.scheduleOnce(50.millis) {
          if (cancellableOpt.forall(_.status.isCancelled)) viewHolderRef ! getRequiredData
          else self ! StartMining
        }
      }

    case pmi: PowMiningInfo =>

      if (!cancellableOpt.forall(_.status.isCancelled)) {
        log.warn("Trying to run miner when the old one is still running")
      } else {
        val difficulty = pmi.powDifficulty
        val bestPowBlock = pmi.bestPowBlock

        log.info(s"Starting new block mining for ${bestPowBlock.encodedId}:${encoder.encodeId(pmi.bestPosId)}")
        // V: In hybrid protocol the "next" PoW block was mined only when the current best PoWblock was paired to a PoS
        // block. Here we always try to mine the next PoW block
        val (parentId, prevPosId, brothers) = (bestPowBlock.id, pmi.bestPosId, Seq()) //new step

        val pubkey = pmi.pubkey

        val p = Promise[Option[PowBlock]]()
        cancellableOpt = Some(Cancellable.run() { status =>
          Future {
            var foundBlock: Option[PowBlock] = None
            var attemps = 0

            while (status.nonCancelled && foundBlock.isEmpty) {
              foundBlock = powIteration(parentId, prevPosId, brothers, difficulty, settings, pubkey, settings.blockGenerationDelay)
              attemps = attemps + 1
              if (attemps % 100 == 99) {
                log.debug(s"100 hashes tried, difficulty is $difficulty")
              }
            }
            p.success(foundBlock)
          }
        })

        p.future.onComplete { toBlock =>
          toBlock.getOrElse(None).foreach { block =>
            log.debug(s"Locally generated PoW block: $block with difficulty $difficulty")
            self ! block
          }
        }
      }


    case b: PowBlock =>
      cancellableOpt.foreach(_.cancel())
      viewHolderRef ! LocallyGeneratedModifier[HybridBlock](b)

    case StopMining =>
      mining = false

    case a: Any =>
      log.warn(s"Strange input: $a")
  }
}

object PowMiner extends App {

  object ReceivableMessages {

    case object StartMining

    case object StopMining

    case object MineBlock

    case class PowMiningInfo(pairCompleted: Boolean,
                             powDifficulty: BigInt,
                             bestPowBlock: PowBlock,
                             bestPosId: ModifierId,
                             pubkey: PublicKey25519Proposition)

  }

  def powIteration(parentId: BlockId,
                   prevPosId: BlockId,
                   brothers: Seq[PowBlockHeader],
                   difficulty: BigInt,
                   settings: HybridMiningSettings,
                   proposition: PublicKey25519Proposition,
                   blockGenerationDelay: FiniteDuration
                  ): Option[PowBlock] = {
    val nonce = Random.nextLong()

    val ts = System.currentTimeMillis()

    val bHash = if (brothers.isEmpty) Array.fill(32)(0: Byte) else Blake2b256(PowBlockCompanion.brotherBytes(brothers))

    val b = PowBlock(parentId, prevPosId, ts, nonce, brothers.size, bHash, proposition, brothers)

    val foundBlock =
      if (b.correctWork(difficulty, settings)) {
        Some(b)
      } else {
        None
      }
    Thread.sleep(blockGenerationDelay.toMillis)
    foundBlock
  }

}

object PowMinerRef {
  def props(viewHolderRef: ActorRef, settings: HybridMiningSettings)(implicit ec: ExecutionContext): Props =
    Props(new PowMiner(viewHolderRef, settings))

  def apply(viewHolderRef: ActorRef, settings: HybridMiningSettings)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(viewHolderRef, settings))

  def apply(name: String, viewHolderRef: ActorRef, settings: HybridMiningSettings)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(viewHolderRef, settings), name)
}
