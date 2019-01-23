package prism1

import java.io.{BufferedWriter, File, FileWriter}

import examples.commons.SimpleBoxTransactionPrismMemPool
import examples.prism1.HybridNodeViewHolder
import examples.prism1.blocks.PowBlock
import examples.prism1.history.{HybridHistory, HybridSyncInfo}
import examples.prism1.mining.HybridSettings
import examples.prism1.state.HBoxStoredState
import examples.prism1.wallet.HBoxWallet
import scorex.core.block.Block
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.hash.Blake2b256
import scorex.util.bytesToId

import scala.util.Random

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Generator {
  type SI = HybridSyncInfo
  type HIS = HybridHistory
  type MS = HBoxStoredState
  type VL = HBoxWallet
  type MP = SimpleBoxTransactionPrismMemPool

  def hybridHistoryGenerator(blockInterval: Int): HIS = {
    val rInPath = Random.nextInt()
    val userConfigPath = "src/main/resources/settings.conf" // whether use this or above path?
    var hybridSettings = HybridSettings.read(Some(userConfigPath))
    val mining = hybridSettings.mining.copy(blockGenerationDelay = blockInterval.seconds)
    val walletSettings = hybridSettings.walletSettings.copy(walletDir = new File(s"/tmp/scorex/tmp$rInPath/wallet"))
    val scorexSettings = hybridSettings.scorexSettings.copy(dataDir = new File(s"/tmp/scorex/tmp$rInPath/blockchain"), logDir = new File(s"/tmp/scorex/tmp$rInPath/log"))
    hybridSettings = HybridSettings(mining, walletSettings, scorexSettings)
    val ntp = new NetworkTimeProvider(hybridSettings.scorexSettings.ntp)
    val noValHistory = HybridHistory.readOrGenerateNoValidation(hybridSettings.scorexSettings, hybridSettings.mining, ntp)
    noValHistory
  }

  def randomPowBlockGenerator(parentId: Block.BlockId, timeStamp: Block.Timestamp): PowBlock = {
    val nonce = Random.nextLong()
    val txs = Seq()
    val txsHash = Array.fill(32)(0: Byte)
    val minerId = bytesToId(Blake2b256("0")) // A fake Id
    val proposition = PrivateKey25519Companion.generateKeys("fake_seed".getBytes)
    PowBlock(parentId,  timeStamp, nonce, proposition._2, txs, txsHash, minerId)
  }
}
