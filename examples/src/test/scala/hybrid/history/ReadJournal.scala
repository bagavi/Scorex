package hybrid.history

import java.io.File

import io.iohk.iodb.LSMStore
import org.scalatest.PropSpec

class ReadJournal extends PropSpec{

  {
//    val dir = "/tmp/scorex/data/blockchain/blocks"
//    val dir = "/tmp/scorex/data/blockchain/state"
    val dir = "/tmp/scorex/data/wallet/wallet.dat"
    val iFile = new File(dir)
    val blockStorage = new LSMStore(iFile, maxJournalEntryCount = 10000)
    blockStorage.getAll((K,V) => println(K,V))
    blockStorage.close()
  }
}
