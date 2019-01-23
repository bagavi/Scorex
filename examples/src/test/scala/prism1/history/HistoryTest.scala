package prism1.history

import examples.prism1.history.HybridHistory
import scorex.util.ModifierId

object HistoryTest {
  def chainIds(hybridHistory: HybridHistory): Seq[ModifierId] = {
    hybridHistory.lastPowBlocks(Int.MaxValue, hybridHistory.bestPowBlock).map(_.id)
  }

  def chainMinerIds(hybridHistory: HybridHistory): Seq[ModifierId] = {
    hybridHistory.lastPowBlocks(Int.MaxValue, hybridHistory.bestPowBlock).map(_.minerId)
  }
}
