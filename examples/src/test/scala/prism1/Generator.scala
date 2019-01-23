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

object Generator {
  type SI = HybridSyncInfo
  type HIS = HybridHistory
  type MS = HBoxStoredState
  type VL = HBoxWallet
  type MP = SimpleBoxTransactionPrismMemPool

  def hybridHistoryGenerator(blockInterval: Int): HIS = {
    val rInPath = Random.nextInt()
    val configText = s"""scorex {
  dataDir = /tmp/scorex/tmp$rInPath/blockchain
  logDir = /tmp/scorex/tmp$rInPath/log

  restApi {
    bindAddress = "127.0.0.1:9085"
    api-key-hash = ""
  }

  network {
    nodeName = "generatorNode1"
    bindAddress = "127.0.0.1:9084"
    knownPeers = []
    agentName = "2-Hop"
  }

  miner {
    offlineGeneration = true
    targetBlockDelay = ${blockInterval}s
    blockGenerationDelay = 100ms
    rParamX10 = 8
    initialDifficulty = 1
    posAttachmentSize = 1
    blockNetworkTransmissionDelay = 1s
    minerNumber = "1"
  }

  wallet {
    seed = "minerNode1"
    password = "cookies"
    walletDir = "/tmp/scorex/tmp$rInPath/wallet"
  }
}
"""
    val userConfigPath = "src/main/resources/tmpsettings.conf"
    val bw = new BufferedWriter(new FileWriter(new File(userConfigPath)))
    bw.write(configText)
    bw.close()
    //    val userConfigPath = "examples/src/main/resources/settings.conf" // whether use this or above path?
    val hybridSettings = HybridSettings.read(Some(userConfigPath))
    val ntp = new NetworkTimeProvider(hybridSettings.scorexSettings.ntp)
//    val ret = HybridNodeViewHolder.generateGenesisState(hybridSettings, ntp)
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
