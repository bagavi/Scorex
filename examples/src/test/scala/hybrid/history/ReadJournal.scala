package hybrid.history

import java.io.File

import examples.hybrid.mining.HybridSettings

import examples.hybrid.history.{HistoryStorage, HybridHistory}
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
    val userConfigPath = "examples/src/main/resources/settings.conf"
    val hybridSettings = HybridSettings.read(Some(userConfigPath))
    val dataDir = hybridSettings.scorexSettings.dataDir
//    println(dataDir)

    val blockStorage = new LSMStore(new File(dataDir + "/blocks"), maxJournalEntryCount = 10000)
//    blockStorage.getAll((K,V) => println(K,V))


    val historyStorage = new HistoryStorage(blockStorage, hybridSettings.mining)
    //we don't care about validation here
    val validators = Seq()
    val hybridHistory = new HybridHistory(historyStorage, hybridSettings.mining, validators, None, new NetworkTimeProvider(hybridSettings.scorexSettings.ntp))
    println(hybridHistory.lastPowBlocks(100, hybridHistory.bestPowBlock))
//    println(historyStorage.getPoWDifficulty(None))
//    println(historyStorage.height)
    blockStorage.close()
  }
}
