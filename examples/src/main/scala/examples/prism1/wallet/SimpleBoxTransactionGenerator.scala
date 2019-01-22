package examples.prism1.wallet

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import examples.commons.{SimpleBoxTransactionPrism, SimpleBoxTransactionPrismMemPool, Value}
import examples.prism1.history.HybridHistory
import examples.prism1.state.HBoxStoredState
import scorex.core.NodeViewHolder.CurrentView
import scorex.util.ScorexLogging

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Random, Success, Try}

/**
  * Generator of SimpleBoxTransactionPrism inside a wallet
  */
class SimpleBoxTransactionPrismGenerator(viewHolderRef: ActorRef)(implicit ec: ExecutionContext) extends Actor
  with ScorexLogging {

  import SimpleBoxTransactionPrismGenerator.ReceivableMessages._
  import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}

  private val getRequiredData: GetDataFromCurrentView[HybridHistory,
    HBoxStoredState,
    HBoxWallet,
    SimpleBoxTransactionPrismMemPool,
    GeneratorInfo] = {
    val f: CurrentView[HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionPrismMemPool] => GeneratorInfo = {
      view: CurrentView[HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionPrismMemPool] =>
        GeneratorInfo(generate(view.vault))
    }
    GetDataFromCurrentView[HybridHistory,
      HBoxStoredState,
      HBoxWallet,
      SimpleBoxTransactionPrismMemPool,
      GeneratorInfo](f)
  }


  override def receive: Receive = {
    case StartGeneration(duration) =>
      context.system.scheduler.schedule(duration, duration, viewHolderRef, getRequiredData)

    //    case CurrentView(_, _, wallet: HWallet, _) =>
    case gi: GeneratorInfo =>
      gi.tx match {
        case Success(tx) =>
          log.info(s"Local tx with with ${tx.from.size} inputs, ${tx.to.size} outputs. Valid: ${tx.semanticValidity}")
          viewHolderRef ! LocallyGeneratedTransaction[SimpleBoxTransactionPrism](tx)
        case Failure(e) =>
          e.printStackTrace()
      }
  }

  private val ex: ArrayBuffer[Array[Byte]] = ArrayBuffer()

  def generate(wallet: HBoxWallet): Try[SimpleBoxTransactionPrism] = {
    if (Random.nextInt(100) == 1) ex.clear()

    val pubkeys = wallet.publicKeys.toSeq
    while (wallet.publicKeys.toSeq.size < 1000) wallet.generateNewSecret()
    val recipients = scala.util.Random.shuffle(pubkeys).take(1)
      .map(r => (r, Value @@ (1 + Random.nextInt(10).toLong)))
    val tx = SimpleBoxTransactionPrism.create(wallet, recipients, 0, ex)
//    tx.map(t => t.boxIdsToOpen.foreach(id => ex += id))
    tx
  }
}

object SimpleBoxTransactionPrismGenerator {

  object ReceivableMessages {

    case class StartGeneration(delay: FiniteDuration)

    case class GeneratorInfo(tx: Try[SimpleBoxTransactionPrism])

  }

}

object SimpleBoxTransactionPrismGeneratorRef {
  def props(viewHolderRef: ActorRef)(implicit ec: ExecutionContext): Props =
    Props(new SimpleBoxTransactionPrismGenerator(viewHolderRef))

  def apply(viewHolderRef: ActorRef)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = system.actorOf(props(viewHolderRef))

  def apply(name: String, viewHolderRef: ActorRef)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = system.actorOf(props(viewHolderRef), name)
}
