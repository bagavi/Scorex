package bitcoin

import examples.commons.{SimpleBoxTransactionBitcoin, SimpleBoxTransactionBitcoinMemPool}
import examples.bitcoin.blocks.BitcoinBlock
import examples.bitcoin.history.{BitcoinHistory, BitcoinSyncInfo}
import examples.bitcoin.state.BitcoinBoxStoredState
import scorex.testkit.properties.NodeViewSynchronizerTests

class NodeViewSynchronizerSpec
  extends NodeViewSynchronizerTests[SimpleBoxTransactionBitcoin, BitcoinBlock, BitcoinBoxStoredState, BitcoinSyncInfo,
                                    BitcoinHistory, SimpleBoxTransactionBitcoinMemPool] with HybridGenerators {

  override lazy val memPool: SimpleBoxTransactionBitcoinMemPool = SimpleBoxTransactionBitcoinMemPool.emptyPool
}
