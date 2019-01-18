package prism1.history

import java.io.{BufferedWriter, File, FileWriter}

import examples.prism1.blocks.{HybridBlock, PowBlock, PowBlockCompanion}
import examples.prism1.history.{HistoryStorage, HybridHistory}
import examples.prism1.mining.HybridSettings
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.scalatest.PropSpec
import scorex.core.utils.NetworkTimeProvider
import scorex.util.idToBytes

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Try}

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
