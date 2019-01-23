package prism1.history

import examples.prism1.history.HybridHistory
import examples.prism1.mining.HybridSettings
import org.scalatest.PropSpec
import prism1.{HistoryGenerators, StoreGenerators}
import scorex.util.ModifierId

class HistoryTest extends PropSpec with HistoryGenerators with StoreGenerators {
  val userConfigPath = "src/main/resources/settings.conf" // whether use this or above path?

  override def settings: HybridSettings = HybridSettings.read(Some(userConfigPath))

  property("should get a history height==1") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val hybridHistory: HybridHistory = historyGen.sample.get
    assert(hybridHistory.height == 1)
  }
}
object HistoryTest {
  def chainIds(hybridHistory: HybridHistory): Seq[ModifierId] = {
    hybridHistory.lastPowBlocks(Int.MaxValue, hybridHistory.bestPowBlock).map(_.id)
  }

  def chainMinerIds(hybridHistory: HybridHistory): Seq[ModifierId] = {
    hybridHistory.lastPowBlocks(Int.MaxValue, hybridHistory.bestPowBlock).map(_.minerId)
  }
}
