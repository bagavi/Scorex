package prism1

import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.prism1.HybridNodeViewHolder
import examples.prism1.blocks._
import examples.prism1.history.{HybridHistory, HybridSyncInfo, HybridSyncInfoMessageSpec}
import examples.prism1.state.HBoxStoredState
import scorex.core.consensus.SyncInfo
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

trait HybridTypes {

  type P = PublicKey25519Proposition
  type TX = SimpleBoxTransaction
  type PM = HybridBlock
  type SI = SyncInfo
  type HSI = HybridSyncInfo
  type SIS = HybridSyncInfoMessageSpec.type

  type NODE = HybridNodeViewHolder
  type ST = HBoxStoredState
  type HT = HybridHistory
  type MP = SimpleBoxTransactionMemPool

}