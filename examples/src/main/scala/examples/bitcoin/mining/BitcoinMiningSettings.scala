package examples.bitcoin.mining

import java.io.File

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader
import scorex.core.bytesToId
import scorex.core.settings.ScorexSettings.readConfigFromPath
import scorex.core.settings._
import scorex.crypto.hash.Blake2b256
import scorex.util.ScorexLogging

import scala.concurrent.duration._

case class BitcoinSettings(mining: BitcoinMiningSettings,
                           walletSettings: WalletSettings,
                           scorexSettings: ScorexSettings)

case class WalletSettings(seed: String,
                          password: String,
                          walletDir: File)

case class BitcoinMiningSettings(offlineGeneration: Boolean,
                                 targetBlockDelay: FiniteDuration,
                                 blockGenerationDelay: FiniteDuration,
                                 posAttachmentSize: Int,
                                 rParamX10: Int,
                                 initialDifficulty: BigInt,
                                 blockNetworkTransmissionDelay: FiniteDuration,
                                 txGenerationRate: FiniteDuration,
                                 minerNumber: String,
                                 txsPerBlock: Int) {
  lazy val MaxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  lazy val GenesisParentId = bytesToId(Array.fill(32)(0: Byte))
  lazy val minerId = bytesToId(Blake2b256(minerNumber))
}

object BitcoinSettings extends ScorexLogging with SettingsReaders {
  def read(userConfigPath: Option[String]): BitcoinSettings = {
    fromConfig(readConfigFromPath(userConfigPath, "scorex"))
  }

  implicit val networkSettingsValueReader: ValueReader[BitcoinSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  private def fromConfig(config: Config): BitcoinSettings = {
    log.debug(config.toString)
    val walletSettings = config.as[WalletSettings]("scorex.wallet")
    val miningSettings = config.as[BitcoinMiningSettings]("scorex.miner")
    val scorexSettings = config.as[ScorexSettings]("scorex")
    BitcoinSettings(miningSettings, walletSettings, scorexSettings)
  }
}

