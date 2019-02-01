package prism1.deprecated

import examples.prism1.blocks.PowBlock
import examples.prism1.history.HybridHistory
import org.scalatest.PropSpec
import Generator.randomPowBlockGenerator
import prism1.HybridGenerators
import scorex.crypto.hash.Blake2b256
import scorex.util.{ModifierId, bytesToId}

import scala.util.{Failure, Success}

class HistoryTest extends PropSpec with HybridGenerators {
//  val userConfigPath = "src/main/resources/settings.conf" // whether use this or above path?

//  override val settings: HybridSettings = HybridSettings.read(Some(userConfigPath))

  ignore("should get a history height==1") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val hybridHistory: HybridHistory = historyGen.sample.get
    assert(hybridHistory.height == 1)
  }

  property("wrong parent id should fail") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    var hybridHistory: HybridHistory = historyGen.sample.get
    val genesisBlock: PowBlock = hybridHistory.bestPowBlock
    val t0 = genesisBlock.timestamp
    var length = 1
    for (i <- 1 to length) {
      val block: PowBlock = randomPowBlockGenerator(bytesToId(Blake2b256(hybridHistory.bestPowId)), t0 + i * 1000)
      hybridHistory.append(block) match {
        case Success((history, progressInfo)) =>
          hybridHistory = history
        case Failure(e) =>
          println(e)
      }
    }
  }
  ignore("longest chain should win") {
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

}
