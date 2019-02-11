package examples.bitcoin.mining

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import examples.commons.SimpleBoxTransactionBitcoinMemPool
import examples.commons.{SimpleBoxTransactionBitcoin, SimpleBoxTransactionBitcoinCompanion}
import examples.bitcoin.blocks.{BitcoinBlock, PowBlock, PowBlockCompanion, PowBlockHeader}
import examples.bitcoin.history.BitcoinHistory
import examples.bitcoin.state.BitcoinBoxStoredState
import examples.bitcoin.util.Cancellable
import examples.bitcoin.wallet.BitcoinBoxWallet
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.block.Block.BlockId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexEncoding
import scorex.crypto.hash.Blake2b256
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Random

/**
  * A controller for PoW mining
  * currently it is starting to work on getting a (PoW; PoS) block references
  * and stops on a new PoW block found (when PoS ref is unknown)
  */
class PowMiner(viewHolderRef: ActorRef, settings: BitcoinMiningSettings)(implicit ec: ExecutionContext)
  extends Actor with ScorexLogging with ScorexEncoding {

  import PowMiner.ReceivableMessages._
  import PowMiner._
  import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}


  private var cancellableOpt: Option[Cancellable] = None
  private var mining = false
  private val TransactionsPerBlock: Int = settings.txsPerBlock
  private val getRequiredData: GetDataFromCurrentView[BitcoinHistory,
    BitcoinBoxStoredState,
    BitcoinBoxWallet,
    SimpleBoxTransactionBitcoinMemPool,
    PowMiningInfo] = {
    val f: CurrentView[BitcoinHistory, BitcoinBoxStoredState, BitcoinBoxWallet, SimpleBoxTransactionBitcoinMemPool] => PowMiningInfo = {
      view: CurrentView[BitcoinHistory, BitcoinBoxStoredState, BitcoinBoxWallet, SimpleBoxTransactionBitcoinMemPool] =>

        val difficulty = view.history.powDifficulty
        val bestPowBlock = view.history.bestPowBlock
         // Pick transactions from the view's mempool
        val txs = view.pool.take(TransactionsPerBlock).foldLeft(Seq[SimpleBoxTransactionBitcoin]()) { case (collected, tx) =>
          val txNonces = tx.from.map{case (_ , nonce ) => nonce}
          val collectedNonces = collected.flatMap(_.from).map{case (_ , nonce ) => nonce}
          if (view.state.validate(tx).isSuccess &&
            //ToDo: Vivek: I have edited this. To fix later.
//            tx.boxIdsToOpen.forall(id => !collected.flatMap(_.boxIdsToOpen).contains(id))) collected :+ tx
            txNonces.forall(nonce => !collectedNonces.contains(nonce))) collected :+ tx
          else collected
        }

        log.info(s"${txs.size} out of ${view.pool.size} transactions added.")
        // TODO: fixme, What should we do if `view.vault.generateNewSecret().publicKeys` is empty?
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val pubkey = view.vault.publicKeys.headOption getOrElse view.vault.generateNewSecret().publicKeys.head
        PowMiningInfo(difficulty, bestPowBlock, pubkey, txs)
    }
    GetDataFromCurrentView[BitcoinHistory,
      BitcoinBoxStoredState,
      BitcoinBoxWallet,
      SimpleBoxTransactionBitcoinMemPool,
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
        log.debug("Mining of previous PoW block stopped")
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

        // V: In hybrid protocol the "next" PoW block was mined only when the current best PoWblock was paired to a PoS
        // block. Here we always try to mine the next PoW block
        val parentId = bestPowBlock.id //new step

        val pubkey = pmi.pubkey
        val txs = pmi.txs

        val p = Promise[Option[PowBlock]]()
        log.info(s"Mining on ${bestPowBlock.encodedId}")
        cancellableOpt = Some(Cancellable.run() { status =>
          Future {
            var foundBlock: Option[PowBlock] = None
            var attempts = 0

            while (status.nonCancelled && foundBlock.isEmpty) {
              foundBlock = powIteration(parentId, difficulty, settings, pubkey, settings.blockGenerationDelay, txs)
              attempts = attempts + 1
              if (attempts % 100 == 99) {
                log.info(s"${attempts} hashes tried, difficulty is $difficulty")
              }
            }
//            log.info("Found new block!")
            p.success(foundBlock)
          }
        })

        p.future.onComplete { toBlock =>
          toBlock.getOrElse(None).foreach { block =>
            log.debug(s"Locally generated PoW block: $block with difficulty $difficulty")
            Thread.sleep(settings.blockNetworkTransmissionDelay.toMillis)
            self ! block
          }
        }
      }

    case b: PowBlock =>
      cancellableOpt.foreach(_.cancel())
      viewHolderRef ! LocallyGeneratedModifier[BitcoinBlock](b)

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

    case class PowMiningInfo(powDifficulty: BigInt,
                             bestPowBlock: PowBlock,
                             pubkey: PublicKey25519Proposition,
                             txs: Seq[SimpleBoxTransactionBitcoin])

  }
  def powIteration(parentId: BlockId,
                   difficulty: BigInt,
                   settings: BitcoinMiningSettings,
                   proposition: PublicKey25519Proposition,
                   blockGenerationDelay: FiniteDuration,
                   txs: Seq[SimpleBoxTransactionBitcoin]
                  ): Option[PowBlock] = {
    val nonce = Random.nextLong()

    val timeStamp = System.currentTimeMillis()

    val txsHash = Blake2b256(PowBlockCompanion.txBytes(txs))

    val b = PowBlock(parentId,  timeStamp, nonce, proposition, txs, txsHash, settings.minerId)

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
  def props(viewHolderRef: ActorRef, settings: BitcoinMiningSettings)(implicit ec: ExecutionContext): Props =
    Props(new PowMiner(viewHolderRef, settings))

  def apply(viewHolderRef: ActorRef, settings: BitcoinMiningSettings)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(viewHolderRef, settings))

  def apply(name: String, viewHolderRef: ActorRef, settings: BitcoinMiningSettings)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(viewHolderRef, settings), name)
}
