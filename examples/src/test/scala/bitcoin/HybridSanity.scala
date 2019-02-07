package bitcoin

import examples.commons.{PublicKey25519NoncedBox, SimpleBoxTransactionBitcoin, SimpleBoxTransactionBitcoinMemPool}
import examples.bitcoin.blocks.{BitcoinBlock, PowBlock}
import examples.bitcoin.history.{BitcoinHistory, BitcoinSyncInfo}
import examples.bitcoin.state.BitcoinBoxStoredState
import examples.bitcoin.wallet.BitcoinBoxWallet
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.testkit.{BlockchainPerformance, BlockchainSanity}


class HybridSanity extends BlockchainSanity[PublicKey25519Proposition,
  SimpleBoxTransactionBitcoin,
  BitcoinBlock,
  PowBlock,
  BitcoinSyncInfo,
  PublicKey25519NoncedBox,
  SimpleBoxTransactionBitcoinMemPool,
  BitcoinBoxStoredState,
  BitcoinHistory] with BlockchainPerformance[SimpleBoxTransactionBitcoin, BitcoinBlock, BitcoinSyncInfo, SimpleBoxTransactionBitcoinMemPool, BitcoinBoxStoredState, BitcoinHistory]
  with HybridGenerators {

  private val walletSettings = originalSettings.walletSettings.copy(seed = "p")

  //Node view components
  override lazy val memPool: SimpleBoxTransactionBitcoinMemPool = SimpleBoxTransactionBitcoinMemPool.emptyPool
  override lazy val memPoolGenerator: Gen[SimpleBoxTransactionBitcoinMemPool] = emptyMemPoolGen
  override lazy val transactionGenerator: Gen[TX] = simpleBoxTransactionBitcoinGen
  override lazy val wallet = (0 until 100).foldLeft(BitcoinBoxWallet.readOrGenerate(walletSettings))((w, _) => w.generateNewSecret())
}