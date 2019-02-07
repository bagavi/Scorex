package examples.bitcoin.wallet

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import examples.commons.{SimpleBoxTransactionBitcoin, SimpleBoxTransactionBitcoinMemPool, Value}
import examples.bitcoin.history.BitcoinHistory
import examples.bitcoin.state.BitcoinBoxStoredState
import scorex.core.NodeViewHolder.CurrentView
import scorex.util.ScorexLogging

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Random, Success, Try}

/**
  * Generator of SimpleBoxTransactionBitcoin inside a wallet
  */
class SimpleBoxTransactionGenerator(viewHolderRef: ActorRef)(implicit ec: ExecutionContext) extends Actor
  with ScorexLogging {

  import SimpleBoxTransactionGenerator.ReceivableMessages._
  import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}

  private val getRequiredData: GetDataFromCurrentView[BitcoinHistory,
    BitcoinBoxStoredState,
    BitcoinBoxWallet,
    SimpleBoxTransactionBitcoinMemPool,
    GeneratorInfo] = {
    val f: CurrentView[BitcoinHistory, BitcoinBoxStoredState, BitcoinBoxWallet, SimpleBoxTransactionBitcoinMemPool] => GeneratorInfo = {
      view: CurrentView[BitcoinHistory, BitcoinBoxStoredState, BitcoinBoxWallet, SimpleBoxTransactionBitcoinMemPool] =>
        GeneratorInfo(generate(view.vault))
    }
    GetDataFromCurrentView[BitcoinHistory,
      BitcoinBoxStoredState,
      BitcoinBoxWallet,
      SimpleBoxTransactionBitcoinMemPool,
      GeneratorInfo](f)
  }


  override def receive: Receive = {
    case StartGeneration(duration) =>
      context.system.scheduler.schedule(duration, duration, viewHolderRef, getRequiredData)

    //    case CurrentView(_, _, wallet: HWallet, _) =>
    case gi: GeneratorInfo =>
      gi.tx match {
        case Success(tx) =>
          log.debug(s"Local tx: ${tx.from.size} inputs, ${tx.to.size} outputs. Valid: ${tx.semanticValidity}")
          viewHolderRef ! LocallyGeneratedTransaction[SimpleBoxTransactionBitcoin](tx)
        case Failure(e) =>
//          e.printStackTrace()
      }
  }

  private val ex: ArrayBuffer[Array[Byte]] = ArrayBuffer()

  def generate(wallet: BitcoinBoxWallet): Try[SimpleBoxTransactionBitcoin] = {
    if (Random.nextInt(100) == 1) ex.clear()

    val pubkeys = wallet.publicKeys.toSeq
    while (wallet.publicKeys.toSeq.size < 20) wallet.generateNewSecret()
    val recipients = scala.util.Random.shuffle(pubkeys).take(Random.nextInt(4))
      .map(r => (r, Value @@ (1 + Random.nextInt(5).toLong)))
    val tx = SimpleBoxTransactionBitcoin.create(wallet, recipients, 1 + Random.nextInt(2), ex)
    tx.map(t => t.boxIdsToOpen.foreach(id => ex += id))
    tx
  }
}

object SimpleBoxTransactionGenerator {

  object ReceivableMessages {

    case class StartGeneration(delay: FiniteDuration)

    case class GeneratorInfo(tx: Try[SimpleBoxTransactionBitcoin])

  }

}

object SimpleBoxTransactionGeneratorRef {
  def props(viewHolderRef: ActorRef)(implicit ec: ExecutionContext): Props =
    Props(new SimpleBoxTransactionGenerator(viewHolderRef))

  def apply(viewHolderRef: ActorRef)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = system.actorOf(props(viewHolderRef))

  def apply(name: String, viewHolderRef: ActorRef)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = system.actorOf(props(viewHolderRef), name)
}
