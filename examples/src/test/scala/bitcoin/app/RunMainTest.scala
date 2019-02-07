package bitcoin.app

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import examples.commons.SimpleBoxTransactionBitcoinMemPool
import examples.bitcoin.BitcoinApp
import examples.bitcoin.history.BitcoinHistory
import examples.bitcoin.state.BitcoinBoxStoredState
import examples.bitcoin.wallet.BitcoinBoxWallet
import org.scalatest.PropSpec
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.util.ModifierId

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.sys.process._

class RunMainTest extends PropSpec {
  /**
    * Please run ConfigGenerator.sh first
    */
  import RunMainTest._
  ignore("Start 2 nodes, wait for 3min, then check 1) two views of chain are consistent. (one is the prefix of the other)" +
    " 2) both 2 nodes mine blocks") {
    "src/main/resources/testbench/ConfigGenerator.sh topology.txt" !

    val app1 = new BitcoinApp("src/main/resources/testbench/settings1.conf")
    val app2 = new BitcoinApp("src/main/resources/testbench/settings2.conf")
    app1.run()
    app2.run()
    Thread.sleep(180000)
    
    val bitcoinHistory1 = getNodeView(app1.nodeViewHolderRef)._1
    val minerIds1 = chainMinerIds(bitcoinHistory1)
    val minerIdMap1 = minerIds1.groupBy(identity).mapValues(_.size)
    val ids1 = chainIds(bitcoinHistory1)

    val bitcoinHistory2 = getNodeView(app2.nodeViewHolderRef)._1
    val minerIds2 = chainMinerIds(bitcoinHistory2)
    val minerIdMap2 = minerIds2.groupBy(identity).mapValues(_.size)
    val ids2 = chainIds(bitcoinHistory2)

    val ids1str = ids1.mkString
    val ids2str = ids2.mkString
    assert(ids1str.startsWith(ids2str) || ids2str.startsWith(ids1str))
    assert(minerIdMap1.contains(app1.bitcoinSettings.mining.minerId))
    assert(minerIdMap1.contains(app2.bitcoinSettings.mining.minerId))
    assert(minerIdMap2.contains(app1.bitcoinSettings.mining.minerId))
    assert(minerIdMap2.contains(app2.bitcoinSettings.mining.minerId))
    val count11 = minerIdMap1.getOrElse(app1.bitcoinSettings.mining.minerId, 0)
    val count12 = minerIdMap1.getOrElse(app2.bitcoinSettings.mining.minerId, 0)
    val count21 = minerIdMap2.getOrElse(app1.bitcoinSettings.mining.minerId, 0)
    val count22 = minerIdMap2.getOrElse(app2.bitcoinSettings.mining.minerId, 0)
    println(s"chain of node1: miner1 ($count11), miner 2 ($count12)")
    println(s"chain of node2: miner1 ($count21), miner 2 ($count22)")
  }

  property("Start 2 nodes, wait for 50s, shutdown one node, wait 20s, restart it, wait 50s. then check 1) two views of chain are consistent. (one is the prefix of the other)" +
    " 2) both 2 nodes mine blocks") {
    "src/main/resources/testbench/ConfigGenerator.sh topology.txt" !

    val app1 = new BitcoinApp("src/main/resources/testbench/settings1.conf")
    val app2 = new BitcoinApp("src/main/resources/testbench/settings2.conf")

    app2.run()
    app1.run()
    Thread.sleep(50000)

    app2.stopNotExit()
    Thread.sleep(20000)

    val app2restart = new BitcoinApp("src/main/resources/testbench/settings2.conf")
    app2restart.run()
    Thread.sleep(50000)

    val bitcoinHistory1 = getNodeView(app1.nodeViewHolderRef)._1
    val minerIds1 = chainMinerIds(bitcoinHistory1)
    val minerIdMap1 = minerIds1.groupBy(identity).mapValues(_.size)
    val ids1 = chainIds(bitcoinHistory1)

    val bitcoinHistory2 = getNodeView(app2restart.nodeViewHolderRef)._1
    val minerIds2 = chainMinerIds(bitcoinHistory2)
    val minerIdMap2 = minerIds2.groupBy(identity).mapValues(_.size)
    val ids2 = chainIds(bitcoinHistory2)

    val ids1str = ids1.mkString
    val ids2str = ids2.mkString
    assert(ids1str.startsWith(ids2str) || ids2str.startsWith(ids1str))
    assert(minerIdMap1.contains(app1.bitcoinSettings.mining.minerId))
    assert(minerIdMap1.contains(app2.bitcoinSettings.mining.minerId))
    assert(minerIdMap2.contains(app1.bitcoinSettings.mining.minerId))
    assert(minerIdMap2.contains(app2.bitcoinSettings.mining.minerId))
    val count11 = minerIdMap1.getOrElse(app1.bitcoinSettings.mining.minerId, 0)
    val count12 = minerIdMap1.getOrElse(app2.bitcoinSettings.mining.minerId, 0)
    val count21 = minerIdMap2.getOrElse(app1.bitcoinSettings.mining.minerId, 0)
    val count22 = minerIdMap2.getOrElse(app2.bitcoinSettings.mining.minerId, 0)
    println(s"chain of node1: miner1 ($count11), miner 2 ($count12)")
    println(s"chain of node2: miner1 ($count21), miner 2 ($count22)")
  }

}

object RunMainTest {

  type NodeView = (BitcoinHistory, BitcoinBoxStoredState, BitcoinBoxWallet, SimpleBoxTransactionBitcoinMemPool)

  private implicit val timeout = Timeout(5 seconds)
  def getNodeView(nvh: ActorRef): NodeView = {
    val future = nvh ? GetDataFromCurrentView[BitcoinHistory,
      BitcoinBoxStoredState,
      BitcoinBoxWallet,
      SimpleBoxTransactionBitcoinMemPool,
      NodeView] { v => (v.history, v.state, v.vault, v.pool) }
    Await.result(future, timeout.duration).asInstanceOf[NodeView]
  }

  def chainIds(bitcoinHistory: BitcoinHistory): Seq[ModifierId] = {
    bitcoinHistory.lastPowBlocks(Int.MaxValue, bitcoinHistory.bestPowBlock).map(_.id)
  }

  def chainMinerIds(bitcoinHistory: BitcoinHistory): Seq[ModifierId] = {
    bitcoinHistory.lastPowBlocks(Int.MaxValue, bitcoinHistory.bestPowBlock).map(_.minerId)
  }
}