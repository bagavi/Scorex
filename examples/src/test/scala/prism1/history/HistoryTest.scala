package prism1.history

import examples.prism1.blocks.PowBlock
import examples.prism1.history.HybridHistory
import examples.prism1.mining.HybridSettings
import org.scalatest.PropSpec
import prism1.Generator.randomPowBlockGenerator
import prism1.{HistoryGenerators, StoreGenerators}
import scorex.util.ModifierId

import scala.util.{Failure, Success}

class HistoryTest extends PropSpec with HistoryGenerators with StoreGenerators {
  val userConfigPath = "src/main/resources/settings.conf" // whether use this or above path?

  override def settings: HybridSettings = HybridSettings.read(Some(userConfigPath))

  property("should get a history height==1") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val hybridHistory: HybridHistory = historyGen.sample.get
    assert(hybridHistory.height == 1)
  }

  property("longest chain should win") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    var hybridHistory: HybridHistory = historyGen.sample.get
    val genesisBlock: PowBlock = hybridHistory.bestPowBlock
    val t0 = genesisBlock.timestamp
    val blockInterval = 2
    var length = 19
    for (i <- 1 to length) {
      val block: PowBlock = randomPowBlockGenerator(hybridHistory.bestPowId, t0 + blockInterval * i * 1000 )
      hybridHistory.append(block) match {
        case Success((history, progressInfo)) =>
          hybridHistory = history
        case Failure(e) =>
          println(e)
      }
    }
    assert(hybridHistory.height == length + 1)
    length = 20
    var fakeBestPowId = genesisBlock.id
    for (i <- 1 to length) {
      val block: PowBlock = randomPowBlockGenerator(fakeBestPowId, t0 + blockInterval * i * 100)
      hybridHistory.append(block) match {
        case Success((history, progressInfo)) =>
          hybridHistory = history
        case Failure(e) =>
          println(e)
      }
      fakeBestPowId = block.id
    }
    assert(hybridHistory.height == length + 1)
    assert(hybridHistory.bestPowId == fakeBestPowId)
    val recordBestPowId = fakeBestPowId
    fakeBestPowId = genesisBlock.id
    for (i <- 1 to length) {
      val block: PowBlock = randomPowBlockGenerator(fakeBestPowId, t0 + blockInterval * i * 100)
      hybridHistory.append(block) match {
        case Success((history, progressInfo)) =>
          hybridHistory = history
        case Failure(e) =>
          println(e)
      }
      fakeBestPowId = block.id
    }
    assert(hybridHistory.height == length + 1)
    assert(hybridHistory.bestPowId == recordBestPowId)
    length = 21
    fakeBestPowId = genesisBlock.id
    for (i <- 1 to length) {
      val block: PowBlock = randomPowBlockGenerator(fakeBestPowId, t0 + blockInterval * i * 100)
      hybridHistory.append(block) match {
        case Success((history, progressInfo)) =>
          hybridHistory = history
        case Failure(e) =>
          println(e)
      }
      fakeBestPowId = block.id
    }
    assert(hybridHistory.height == length + 1)
    assert(hybridHistory.bestPowId == fakeBestPowId)
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
