package bitcoin

import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.bitcoin.blocks.BitcoinBlock
import examples.bitcoin.history.{BitcoinHistory, BitcoinSyncInfo}
import examples.bitcoin.state.BitcoinBoxStoredState
import examples.bitcoin.wallet.BitcoinBoxWallet
import scorex.testkit.properties.NodeViewHolderTests

class NodeViewHolderSpec extends NodeViewHolderTests[SimpleBoxTransaction, BitcoinBlock, BitcoinBoxStoredState,
                                                      BitcoinSyncInfo, BitcoinHistory, SimpleBoxTransactionMemPool]
  with HybridGenerators {
  type VL = BitcoinBoxWallet
}
