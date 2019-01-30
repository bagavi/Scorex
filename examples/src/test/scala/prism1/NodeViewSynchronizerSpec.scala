package prism1

import examples.commons.{SimpleBoxTransactionPrism, SimpleBoxTransactionPrismMemPool}
import examples.prism1.blocks.HybridBlock
import examples.prism1.history.{HybridHistory, HybridSyncInfo}
import examples.prism1.state.HBoxStoredState
import scorex.testkit.properties.NodeViewSynchronizerTests

class NodeViewSynchronizerSpec
  extends NodeViewSynchronizerTests[SimpleBoxTransactionPrism, HybridBlock, HBoxStoredState, HybridSyncInfo,
                                    HybridHistory, SimpleBoxTransactionPrismMemPool] with HybridGenerators {

  override lazy val memPool: SimpleBoxTransactionPrismMemPool = SimpleBoxTransactionPrismMemPool.emptyPool
}
