package bitcoin.deprecated

import java.io.{BufferedWriter, File, FileWriter}

import examples.bitcoin.blocks.PowBlock
import examples.bitcoin.history.{HistoryStorage, BitcoinHistory}
import examples.bitcoin.mining.BitcoinSettings
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
    val hybridSettings = BitcoinSettings.read(Some(userConfigPath))
    val dataDir = hybridSettings.scorexSettings.dataDir

    val blockStorage = new LSMStore(new File(dataDir + "/blocks"), maxJournalEntryCount = 10000)
//    blockStorage.getAll((K,V) => println(K,V))


    val historyStorage = new HistoryStorage(blockStorage, hybridSettings.mining)
    //we don't care about validation here
    val validators = Seq()
    val hybridHistory = new BitcoinHistory(historyStorage, hybridSettings.mining, validators, None, new NetworkTimeProvider(hybridSettings.scorexSettings.ntp))
    val bw = new BufferedWriter(new FileWriter(new File(dataDir + "/blocks/test.txt")))
    val it = blockStorage.getAll()
    it.foreach{ kv =>
      val bw = kv._2
      val bytes = bw.data
      val mtypeId = bytes.head
      val isBlock = mtypeId == PowBlock.ModifierTypeId
      println(mtypeId, isBlock)
//      val parsed: Try[HybridBlock] = mtypeId match {
//        case t: Byte if t == PowBlock.ModifierTypeId =>
//          PowBlockCompanion.parseBytes(bytes.tail)
//        case _ => Failure(new  Throwable("Test"))
//      }
//      parsed match {
//        case Failure(e) => println("Failed to parse bytes from bd", e)
//        case _ =>
//      }
//      println(bytes,mtypeId,parsed.toOption)
    }

    blockStorage.close()
  }
}
