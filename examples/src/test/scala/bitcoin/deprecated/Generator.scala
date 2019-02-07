package bitcoin.deprecated

import examples.bitcoin.blocks.{PowBlock, PowBlockCompanion}
import examples.bitcoin.history.{BitcoinHistory, BitcoinSyncInfo}
import examples.bitcoin.state.BitcoinBoxStoredState
import examples.bitcoin.wallet.BitcoinBoxWallet
import examples.commons.SimpleBoxTransactionBitcoinMemPool
import scorex.core.block.Block
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.hash.Blake2b256
import scorex.util.bytesToId

import scala.util.Random

object Generator {
  type SI = BitcoinSyncInfo
  type HIS = BitcoinHistory
  type MS = BitcoinBoxStoredState
  type VL = BitcoinBoxWallet
  type MP = SimpleBoxTransactionBitcoinMemPool

  def randomPowBlockGenerator(parentId: Block.BlockId, timeStamp: Block.Timestamp): PowBlock = {
    val nonce = Random.nextLong()
    val txs = Seq()
    val txsHash = Blake2b256(PowBlockCompanion.txBytes(txs))
    val minerId = bytesToId(Blake2b256("0")) // A fake Id
    val proposition = PrivateKey25519Companion.generateKeys("fake_seed".getBytes)
    PowBlock(parentId,  timeStamp, nonce, proposition._2, txs, txsHash, minerId)
  }
}
