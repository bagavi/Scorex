package hybrid.history

import java.io.File
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader
import examples.hybrid.mining.{HybridMiningSettings, WalletSettings}
import examples.hybrid.history.HistoryStorage
import io.iohk.iodb.LSMStore
import org.scalatest.PropSpec
import scorex.core.settings.ScorexSettings
import scorex.core.settings.ScorexSettings.readConfigFromPath

class ReadJournal extends PropSpec{
  /**
    * Read Journal file such as blocks or state or wallet.
    * Then output all the key-value pairs.
    * TODO: understand the output.
    */
  {
    val userConfigPath = Some("examples/src/main/resources/settings.conf")
    val config = readConfigFromPath(userConfigPath, "scorex")
    val dataDir = config.getString("scorex.dataDir")
    val miningSettings = config.as[HybridMiningSettings]("scorex.miner")
    val blockStorage = new LSMStore(new File(dataDir + "/blocks"), maxJournalEntryCount = 10000)
    //    val dir = "/tmp/scorex/data/blockchain/blocks"
//    val dir = "/tmp/scorex/data/blockchain/state"
//    val dir = "/tmp/scorex/data/wallet/wallet.dat"
//    blockStorage.getAll((K,V) => println(K,V))

//    val walletSettings = config.as[WalletSettings]("scorex.wallet")
//    val scorexSettings = config.as[ScorexSettings]("scorex")

    val historyStorage = new HistoryStorage(blockStorage, miningSettings)
    //TODO
    blockStorage.close()
  }
}
