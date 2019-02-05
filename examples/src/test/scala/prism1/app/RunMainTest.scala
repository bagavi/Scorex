package prism1.app

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import examples.commons.SimpleBoxTransactionPrismMemPool
import examples.prism1.PrismV1App
import examples.prism1.history.HybridHistory
import examples.prism1.mining.HybridSettings
import examples.prism1.state.HBoxStoredState
import examples.prism1.wallet.HBoxWallet
import org.scalatest.PropSpec
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ModifierId

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class RunMainTest extends PropSpec {
  /**
    * Please run ConfigGenerator.sh first
    */
  import RunMainTest._
  property("Start 2 nodes, wait for 3min, then check 1) two views of chain are consistent. (one is the prefix of the other)" +
    " 2) both 2 nodes mine blocks") {

    val app1 = new PrismV1App("src/main/resources/testbench/settings1.conf")
    val app2 = new PrismV1App("src/main/resources/testbench/settings2.conf")
    app1.run()
    app2.run()
    Thread.sleep(180000)
    
    val hybridHistory1 = getNodeView(app1.nodeViewHolderRef)._1
    val minerIds1 = chainMinerIds(hybridHistory1)
    val minerIdMap1 = minerIds1.groupBy(identity).mapValues(_.size)
    val ids1 = chainIds(hybridHistory1)

    val hybridHistory2 = getNodeView(app2.nodeViewHolderRef)._1
    val minerIds2 = chainMinerIds(hybridHistory2)
    val minerIdMap2 = minerIds2.groupBy(identity).mapValues(_.size)
    val ids2 = chainIds(hybridHistory2)

    val ids1str = ids1.mkString
    val ids2str = ids2.mkString
    assert(ids1str.startsWith(ids2str) || ids2str.startsWith(ids1str))
    assert(minerIdMap1.contains(app1.hybridSettings.mining.minerId))
    assert(minerIdMap1.contains(app2.hybridSettings.mining.minerId))
    assert(minerIdMap2.contains(app1.hybridSettings.mining.minerId))
    assert(minerIdMap2.contains(app2.hybridSettings.mining.minerId))
    val count11 = minerIdMap1.getOrElse(app1.hybridSettings.mining.minerId, 0)
    val count12 = minerIdMap1.getOrElse(app2.hybridSettings.mining.minerId, 0)
    val count21 = minerIdMap2.getOrElse(app1.hybridSettings.mining.minerId, 0)
    val count22 = minerIdMap2.getOrElse(app2.hybridSettings.mining.minerId, 0)
    println(s"chain of node1: miner1 ($count11), miner 2 ($count12)")
    println(s"chain of node2: miner1 ($count21), miner 2 ($count22)")
  }

  ignore("Start 2 nodes, wait for 50s, shutdown one node, wait 20s, restart it, wait 50s. then check 1) two views of chain are consistent. (one is the prefix of the other)" +
    " 2) both 2 nodes mine blocks") {

    val app1 = new PrismV1App("src/main/resources/testbench/settings1.conf")
    val app2 = new PrismV1App("src/main/resources/testbench/settings2.conf")

    app2.run()
    app1.run()
    Thread.sleep(50000)

    app2.stopNotExit()
    Thread.sleep(20000)

    val app2restart = new PrismV1App("src/main/resources/testbench/settings2.conf")
    app2restart.run()
    Thread.sleep(50000)

    val hybridHistory1 = getNodeView(app1.nodeViewHolderRef)._1
    val minerIds1 = chainMinerIds(hybridHistory1)
    val minerIdMap1 = minerIds1.groupBy(identity).mapValues(_.size)
    val ids1 = chainIds(hybridHistory1)

    val hybridHistory2 = getNodeView(app2restart.nodeViewHolderRef)._1
    val minerIds2 = chainMinerIds(hybridHistory2)
    val minerIdMap2 = minerIds2.groupBy(identity).mapValues(_.size)
    val ids2 = chainIds(hybridHistory2)

    val ids1str = ids1.mkString
    val ids2str = ids2.mkString
    assert(ids1str.startsWith(ids2str) || ids2str.startsWith(ids1str))
    assert(minerIdMap1.contains(app1.hybridSettings.mining.minerId))
    assert(minerIdMap1.contains(app2.hybridSettings.mining.minerId))
    assert(minerIdMap2.contains(app1.hybridSettings.mining.minerId))
    assert(minerIdMap2.contains(app2.hybridSettings.mining.minerId))
    val count11 = minerIdMap1.getOrElse(app1.hybridSettings.mining.minerId, 0)
    val count12 = minerIdMap1.getOrElse(app2.hybridSettings.mining.minerId, 0)
    val count21 = minerIdMap2.getOrElse(app1.hybridSettings.mining.minerId, 0)
    val count22 = minerIdMap2.getOrElse(app2.hybridSettings.mining.minerId, 0)
    println(s"chain of node1: miner1 ($count11), miner 2 ($count12)")
    println(s"chain of node2: miner1 ($count21), miner 2 ($count22)")
  }

}

object RunMainTest {

  type NodeView = (HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionPrismMemPool)

  private implicit val timeout = Timeout(5 seconds)
  def getNodeView(nvh: ActorRef): NodeView = {
    val future = nvh ? GetDataFromCurrentView[HybridHistory,
      HBoxStoredState,
      HBoxWallet,
      SimpleBoxTransactionPrismMemPool,
      NodeView] { v => (v.history, v.state, v.vault, v.pool) }
    Await.result(future, timeout.duration).asInstanceOf[NodeView]
  }
  def hybridHistoryGenerator(hybridSettings: HybridSettings): HybridHistory = {
    HybridHistory.readOrGenerateNoValidation(hybridSettings.scorexSettings, hybridSettings.mining, new NetworkTimeProvider(hybridSettings.scorexSettings.ntp))
  }

  def hybridHistoryGenerator(userConfigPath: String): HybridHistory = {
    val hybridSettings = HybridSettings.read(Some(userConfigPath))
    hybridHistoryGenerator(hybridSettings)
  }
  def chainIds(hybridHistory: HybridHistory): Seq[ModifierId] = {
    hybridHistory.lastPowBlocks(Int.MaxValue, hybridHistory.bestPowBlock).map(_.id)
  }

  def chainMinerIds(hybridHistory: HybridHistory): Seq[ModifierId] = {
    hybridHistory.lastPowBlocks(Int.MaxValue, hybridHistory.bestPowBlock).map(_.minerId)
  }
}