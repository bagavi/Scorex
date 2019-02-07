package examples.bitcoin

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import examples.bitcoin.blocks.{PowBlock}
import examples.bitcoin.mining.BitcoinMiningSettings
import scorex.core.network.NodeViewSynchronizer.Events.{BetterNeighbourAppeared, NoBetterNeighbour, NodeViewSynchronizerEvent}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{NodeViewHolderEvent, RollbackFailed, SemanticallySuccessfulModifier}
import scorex.util.ScorexLogging

class BitcoinLocalInterface(viewHolderRef: ActorRef,
                            powMinerRef: ActorRef,
                            minerSettings: BitcoinMiningSettings) extends Actor with ScorexLogging {

  import examples.bitcoin.mining.PowMiner.ReceivableMessages.{MineBlock, StartMining, StopMining}

  private var block = false


  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[NodeViewHolderEvent])
    context.system.eventStream.subscribe(self, classOf[NodeViewSynchronizerEvent])
  }

  override def receive: Receive = {
    case RollbackFailed => log.error("Too deep rollback occurred!")

    //start PoW miner to mine next block if a block comes
    case sems: SemanticallySuccessfulModifier[_] =>
      if (!block) {
        sems.modifier match {
          case wb: PowBlock =>
            powMinerRef ! MineBlock
        }
      }

    case NoBetterNeighbour =>
      powMinerRef ! StartMining
      block = false

    case BetterNeighbourAppeared =>
      powMinerRef ! StopMining
      block = true
  }
}

object BitcoinLocalInterfaceRef {
  def props(viewHolderRef: ActorRef,
            powMinerRef: ActorRef,
            minerSettings: BitcoinMiningSettings): Props =
    Props(new BitcoinLocalInterface(viewHolderRef, powMinerRef, minerSettings))

  def apply(viewHolderRef: ActorRef,
            powMinerRef: ActorRef,
            minerSettings: BitcoinMiningSettings)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(viewHolderRef, powMinerRef, minerSettings))

  def apply(name: String, viewHolderRef: ActorRef,
            powMinerRef: ActorRef,
            minerSettings: BitcoinMiningSettings)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(viewHolderRef, powMinerRef, minerSettings), name)
}
