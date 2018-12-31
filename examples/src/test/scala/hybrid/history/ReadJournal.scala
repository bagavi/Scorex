package hybrid.history

import java.io.File

import io.iohk.iodb.LSMStore
import org.scalatest.PropSpec

class ReadJournal extends PropSpec{
  /**
    * Read Journal file such as blocks or state or wallet.
    * Then output all the key-value pairs.
    * TODO: understand the output.
    */
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
