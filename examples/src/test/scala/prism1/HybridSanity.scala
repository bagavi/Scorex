package prism1

import examples.commons.{PublicKey25519NoncedBox, SimpleBoxTransactionPrism, SimpleBoxTransactionPrismMemPool}
import examples.prism1.blocks.{HybridBlock, PowBlock}
import examples.prism1.history.{HybridHistory, HybridSyncInfo}
import examples.prism1.state.HBoxStoredState
import examples.prism1.wallet.HBoxWallet
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.testkit.{BlockchainPerformance, BlockchainSanity}


class HybridSanity extends BlockchainSanity[PublicKey25519Proposition,
  SimpleBoxTransactionPrism,
  HybridBlock,
  PowBlock,
  HybridSyncInfo,
  PublicKey25519NoncedBox,
  SimpleBoxTransactionPrismMemPool,
  HBoxStoredState,
  HybridHistory] with BlockchainPerformance[SimpleBoxTransactionPrism, HybridBlock, HybridSyncInfo, SimpleBoxTransactionPrismMemPool, HBoxStoredState, HybridHistory]
  with HybridGenerators {

  private val walletSettings = originalSettings.walletSettings.copy(seed = "p")

  //Node view components
  override lazy val memPool: SimpleBoxTransactionPrismMemPool = SimpleBoxTransactionPrismMemPool.emptyPool
  override lazy val memPoolGenerator: Gen[SimpleBoxTransactionPrismMemPool] = emptyMemPoolGen
  override lazy val transactionGenerator: Gen[TX] = simpleBoxTransactionPrismGen
  override lazy val wallet = (0 until 100).foldLeft(HBoxWallet.readOrGenerate(walletSettings))((w, _) => w.generateNewSecret())
}