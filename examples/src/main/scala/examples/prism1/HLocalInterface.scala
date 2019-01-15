package examples.prism1

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import examples.prism1.blocks.{PosBlock, PowBlock}
import examples.prism1.mining.HybridMiningSettings
import scorex.core.network.NodeViewSynchronizer.Events.{BetterNeighbourAppeared, NoBetterNeighbour, NodeViewSynchronizerEvent}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{NodeViewHolderEvent, RollbackFailed, SemanticallySuccessfulModifier}
import scorex.util.ScorexLogging

class HLocalInterface(viewHolderRef: ActorRef,
                      powMinerRef: ActorRef,
                      minerSettings: HybridMiningSettings) extends Actor with ScorexLogging {

  import examples.prism1.mining.PosForger.ReceivableMessages.{StartForging, StopForging}
  import examples.prism1.mining.PowMiner.ReceivableMessages.{MineBlock, StartMining, StopMining}

  private var block = false


  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[NodeViewHolderEvent])
    context.system.eventStream.subscribe(self, classOf[NodeViewSynchronizerEvent])
  }

  override def receive: Receive = {
    case RollbackFailed => log.error("Too deep rollback occurred!")

    //stop PoW miner and start PoS forger if PoW block comes
    //stop PoW forger and start PoW miner if PoS block comes
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

object HLocalInterfaceRef {
  def props(viewHolderRef: ActorRef,
            powMinerRef: ActorRef,
            minerSettings: HybridMiningSettings): Props =
    Props(new HLocalInterface(viewHolderRef, powMinerRef, minerSettings))

  def apply(viewHolderRef: ActorRef,
            powMinerRef: ActorRef,
            minerSettings: HybridMiningSettings)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(viewHolderRef, powMinerRef, minerSettings))

  def apply(name: String, viewHolderRef: ActorRef,
            powMinerRef: ActorRef,
            minerSettings: HybridMiningSettings)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(viewHolderRef, powMinerRef, minerSettings), name)
}
