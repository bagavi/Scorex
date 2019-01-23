package prism1.difficulty

import java.io.File

import examples.prism1.PrismV1App
import examples.prism1.history.{HistoryStorage, HybridHistory}
import io.iohk.iodb.LSMStore
import org.scalatest.PropSpec
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext.Implicits.global

class RunMainTest extends PropSpec {
  /**
    * Please rm data before test
    */
  property("See whether offlineGeneration=true will mine blocks(both of 2 nodes)") {

    val app1 = new PrismV1App("src/main/resources/settings.conf")
    val app2 = new PrismV1App("src/main/resources/settings2.conf")
    app1.run()
    app2.run()
    println("two nodes started, now sleeping for a long time")
    Thread.sleep(180000)

    val validators = Seq()      //we don't care about validation here

    val hybridSettings1 = app1.hybridSettings
    val dataDir1 = hybridSettings1.scorexSettings.dataDir
    val blockStorage1 = new LSMStore(new File(dataDir1 + "/blocks"), maxJournalEntryCount = 10000)
    val historyStorage1 = new HistoryStorage(blockStorage1, hybridSettings1.mining)
    val hybridHistory1 = new HybridHistory(historyStorage1, hybridSettings1.mining, validators, None, new NetworkTimeProvider(hybridSettings1.scorexSettings.ntp))
    val minerIds1 = hybridHistory1.lastPowBlocks(Int.MaxValue, hybridHistory1.bestPowBlock).map(_.minerId).toSet
    val ids1 = hybridHistory1.lastPowBlocks(Int.MaxValue, hybridHistory1.bestPowBlock).map(_.id)



    val hybridSettings2 = app2.hybridSettings
    val dataDir2 = hybridSettings2.scorexSettings.dataDir
    val blockStorage2 = new LSMStore(new File(dataDir2 + "/blocks"), maxJournalEntryCount = 10000)
    val historyStorage2 = new HistoryStorage(blockStorage2, hybridSettings2.mining)
    val hybridHistory2 = new HybridHistory(historyStorage2, hybridSettings2.mining, validators, None, new NetworkTimeProvider(hybridSettings2.scorexSettings.ntp))
    val minerIds2 = hybridHistory2.lastPowBlocks(Int.MaxValue, hybridHistory2.bestPowBlock).map(_.minerId).toSet
    val ids2 = hybridHistory2.lastPowBlocks(Int.MaxValue, hybridHistory2.bestPowBlock).map(_.id)

    val b1 = ids1.lastOption match {
      case Some(id) => ids2.contains(id)
      case _ => false
    }
    val b2 = ids2.lastOption match {
        case Some(id) => ids1.contains(id)
        case _ => false
    }
    assert(b1 || b2)
    assert(minerIds1.contains(app1.hybridSettings.mining.minerId))
    assert(minerIds1.contains(app2.hybridSettings.mining.minerId))
    assert(minerIds2.contains(app1.hybridSettings.mining.minerId))
    assert(minerIds2.contains(app2.hybridSettings.mining.minerId))

//    val url = s"curl -s -X GET --header 'Accept: application/json' 'http://${app1.settings.network.bindAddress}/debug/allblocks'"
//    val url = s"http://${app1.settings.restApi.bindAddress.toString.stripPrefix("/")}/debug/allblocks"
//    val allblocks1 = scala.io.Source.fromURL(url).mkString
//    println(allblocks1)
//    println(app1.settings.restApi.bindAddress.toString.stripPrefix("/"))
    blockStorage1.close()
    blockStorage2.close()
  }

  //TODO: see whether offlineGeneration=false will mine
}
