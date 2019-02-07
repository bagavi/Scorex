package examples.bitcoin.blocks

import examples.commons.SimpleBoxTransactionBitcoin
import scorex.core.PersistentNodeViewModifier
import scorex.core.block.Block

trait BitcoinBlock extends PersistentNodeViewModifier with Block[SimpleBoxTransactionBitcoin]
