package prism1.history

import java.io.File

import examples.prism1.history.{HistoryStorage, HybridHistory}
import examples.prism1.mining.HybridSettings
import io.iohk.iodb.LSMStore
import org.scalatest.PropSpec
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext.Implicits.global

class ReadJournal extends PropSpec{
  /**
    * Read Journal file such as blocks or state or wallet.
    * Then output all the key-value pairs.
    * TODO: understand the output.
    */
  {
    val userConfigPath = "src/main/resources/settings.conf"
//    val userConfigPath = "examples/src/main/resources/settings.conf" // whether use this or above path?
    val hybridSettings = HybridSettings.read(Some(userConfigPath))
    val dataDir = hybridSettings.scorexSettings.dataDir

    val blockStorage = new LSMStore(new File(dataDir + "/blocks"), maxJournalEntryCount = 10000)
//    blockStorage.getAll((K,V) => println(K,V))


    val historyStorage = new HistoryStorage(blockStorage, hybridSettings.mining)
    //we don't care about validation here
    val validators = Seq()
    val hybridHistory = new HybridHistory(historyStorage, hybridSettings.mining, validators, None, new NetworkTimeProvider(hybridSettings.scorexSettings.ntp))
    println(hybridHistory.lastPowBlocks(100, hybridHistory.bestPowBlock))

    blockStorage.close()
  }
}
