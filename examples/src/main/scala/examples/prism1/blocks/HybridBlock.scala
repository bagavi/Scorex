package examples.prism1.blocks

import examples.commons.SimpleBoxTransactionPrism
import scorex.core.PersistentNodeViewModifier
import scorex.core.block.Block

trait HybridBlock extends PersistentNodeViewModifier with Block[SimpleBoxTransactionPrism]
