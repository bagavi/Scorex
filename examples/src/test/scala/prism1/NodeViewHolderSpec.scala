package prism1

import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.prism1.blocks.HybridBlock
import examples.prism1.history.{HybridHistory, HybridSyncInfo}
import examples.prism1.state.HBoxStoredState
import examples.prism1.wallet.HBoxWallet
import scorex.testkit.properties.NodeViewHolderTests

class NodeViewHolderSpec extends NodeViewHolderTests[SimpleBoxTransaction, HybridBlock, HBoxStoredState,
                                                      HybridSyncInfo, HybridHistory, SimpleBoxTransactionMemPool]
  with HybridGenerators {
  type VL = HBoxWallet
}
