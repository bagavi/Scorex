package examples.prism1.history

import java.io.{BufferedWriter, File, FileWriter}
import java.util.Calendar

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import examples.prism1.blocks.HybridBlock

import scala.concurrent.ExecutionContext

class HybridHistoryVisualizer(dir: String)(implicit ec: ExecutionContext) extends Actor {
  import HybridHistoryVisualizer._
  protected val dir1 = dir.stripSuffix("/")
//  import context.dispatcher
//  protected val visualizeTimer = context.system.scheduler.schedule(5 second, 10 seconds, self, VisualizeToFile)

//  override def postStop(): Unit = {
//    visualizeTimer.cancel()
//  }

  override def receive: Receive = {
    case VisualizeToFile(blocks) => visualizeToFile(blocks)
  }

  def visualizeToFile(blocks: Seq[HybridBlock]): Unit = {

    val bw = new BufferedWriter(new FileWriter(new File(dir1 + "/readable.txt")))
    bw.write(Calendar.getInstance().getTime.toString)
    bw.write(System.getProperty("line.separator"))
    blocks.foreach {
      line => bw.write(line.toString)
        bw.write(System.getProperty("line.separator"))
    }
    bw.close()
  }
}

object HybridHistoryVisualizer {
  case object VisualizeClock
  final case class VisualizeToFile(blocks: Seq[HybridBlock])

  def props(dir: String)(implicit ec: ExecutionContext): Props =
    Props(new HybridHistoryVisualizer(dir))
}
