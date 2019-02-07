package bitcoin

import examples.commons.{SimpleBoxTransactionBitcoin, SimpleBoxTransactionBitcoinMemPool}
import examples.bitcoin.BitcoinNodeViewHolder
import examples.bitcoin.blocks._
import examples.bitcoin.history.{BitcoinHistory, BitcoinSyncInfo, BitcoinSyncInfoMessageSpec}
import examples.bitcoin.state.BitcoinBoxStoredState
import scorex.core.consensus.SyncInfo
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

trait HybridTypes {

  type P = PublicKey25519Proposition
  type TX = SimpleBoxTransactionBitcoin
  type PM = BitcoinBlock
  type SI = SyncInfo
  type HSI = BitcoinSyncInfo
  type SIS = BitcoinSyncInfoMessageSpec.type

  type NODE = BitcoinNodeViewHolder
  type ST = BitcoinBoxStoredState
  type HT = BitcoinHistory
  type MP = SimpleBoxTransactionBitcoinMemPool

}