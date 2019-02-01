package prism1.deprecated

import java.io.File

import examples.commons.SimpleBoxTransactionPrismMemPool
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

object Generator {
  type SI = HybridSyncInfo
  type HIS = HybridHistory
  type MS = HBoxStoredState
  type VL = HBoxWallet
  type MP = SimpleBoxTransactionPrismMemPool

  def randomPowBlockGenerator(parentId: Block.BlockId, timeStamp: Block.Timestamp): PowBlock = {
    val nonce = Random.nextLong()
    val txs = Seq()
    val txsHash = Array.fill(32)(0: Byte)
    val minerId = bytesToId(Blake2b256("0")) // A fake Id
    val proposition = PrivateKey25519Companion.generateKeys("fake_seed".getBytes)
    PowBlock(parentId,  timeStamp, nonce, proposition._2, txs, txsHash, minerId)
  }
}
