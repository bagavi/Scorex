package prism1.difficulty

import java.io.File

import examples.prism1.PrismV1App
import examples.prism1.history.{HistoryStorage, HybridHistory}
import io.iohk.iodb.LSMStore
import org.scalatest.PropSpec
import prism1.Generator
import prism1.history.HistoryTest
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext.Implicits.global

class RunMainTest extends PropSpec {
  /**
    * Please run configGenerator.sh first
    */
  property("See whether offlineGeneration=true will mine blocks(both of 2 nodes)") {

    val app1 = new PrismV1App("src/main/resources/testbench/settings1.conf")
    val app2 = new PrismV1App("src/main/resources/testbench/settings2.conf")
    app1.run()
    app2.run()
    println("two nodes started, now sleeping for a long time")
    Thread.sleep(90000)

    val hybridHistory1 = Generator.hybridHistoryGenerator(app1.hybridSettings)
    val minerIds1 = HistoryTest.chainMinerIds(hybridHistory1)
    val minerIdMap1 = minerIds1.groupBy(identity).mapValues(_.size)
    val ids1 = HistoryTest.chainIds(hybridHistory1)

    val hybridHistory2 = Generator.hybridHistoryGenerator(app2.hybridSettings)
    val minerIds2 = HistoryTest.chainMinerIds(hybridHistory2)
    val minerIdMap2 = minerIds2.groupBy(identity).mapValues(_.size)
    val ids2 = HistoryTest.chainIds(hybridHistory2)

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
//    val url = s"curl -s -X GET --header 'Accept: application/json' 'http://${app1.settings.network.bindAddress}/debug/allblocks'"
//    val url = s"http://${app1.settings.restApi.bindAddress.toString.stripPrefix("/")}/debug/allblocks"
//    val allblocks1 = scala.io.Source.fromURL(url).mkString
//    println(allblocks1)
//    println(app1.settings.restApi.bindAddress.toString.stripPrefix("/"))
//    blockStorage1.close()
//    blockStorage2.close()
  }

  //TODO: see whether offlineGeneration=false will mine
}
