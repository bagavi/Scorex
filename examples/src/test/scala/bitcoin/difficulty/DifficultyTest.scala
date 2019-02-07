package bitcoin.difficulty

import examples.bitcoin.blocks.PowBlock
import examples.bitcoin.history.BitcoinHistory
import org.scalatest.PropSpec
import bitcoin.BitcoinGenerators

import scala.util.{Failure, Random, Success}

class DifficultyTest extends PropSpec with BitcoinGenerators{

  property("Generate 10 random blocks, should get a BitcoinHistory of height 11") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    var bitcoinHistory: BitcoinHistory = historyGen.sample.get

    val blockInterval: Int = 3 // 3 is default in Generator
    val t0 = bitcoinHistory.bestPowBlock.timestamp

    for (i <- 1 to 10) {
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val block: PowBlock = powBlockGen.sample.get
      val newBlock = block.copy(parentId = bitcoinHistory.bestPowId, timestamp = t0 + blockInterval * i * 1000)
      bitcoinHistory.append(newBlock) match {
        case Success((history, progressInfo)) =>
          bitcoinHistory = history
        case Failure(e) =>
          println(e)
      }
    }
    assert(bitcoinHistory.height == 10 + 1)
  }

  property("Should get a stable difficulty when block generation is exactly block interval") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    var bitcoinHistory: BitcoinHistory = historyGen.sample.get

    val blockInterval: Int = 3
    val t0 = bitcoinHistory.bestPowBlock.timestamp
    val length = 100
    for (i <- 1 to length) {
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val block: PowBlock = powBlockGen.sample.get
      val newBlock = block.copy(parentId = bitcoinHistory.bestPowId, timestamp = t0 + blockInterval * i * 1000)
      bitcoinHistory.append(newBlock) match {
        case Success((history, progressInfo)) =>
          bitcoinHistory = history
        case Failure(e) =>
          println(e)
      }
    }
    val diffs: Seq[BigInt] = bitcoinHistory.lastBlockIds(bitcoinHistory.bestBlock, length).map(id => bitcoinHistory.storage.getPoWDifficulty(Some(id)))
    val minmax = diffs.foldLeft((diffs(0).toInt, diffs(0).toInt)) { case ((min, max), e) => (scala.math.min(min, e.toInt), scala.math.max(max, e.toInt))}
    assert(minmax._2-minmax._1 == 0)
  }

  property("Should get an explosing difficulty when block generation is very quick") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    var bitcoinHistory: BitcoinHistory = historyGen.sample.get

    val blockInterval: Int = 3
    val t0 = bitcoinHistory.bestPowBlock.timestamp
    val length = 100
    for (i <- 1 to length) {
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val block: PowBlock = powBlockGen.sample.get
      val newBlock = block.copy(parentId = bitcoinHistory.bestPowId, timestamp = t0 + blockInterval * i * 1)
      bitcoinHistory.append(newBlock) match {
        case Success((history, progressInfo)) =>
          bitcoinHistory = history
        case Failure(e) =>
          println(e)
      }
    }
    val diffs: Seq[BigInt] = bitcoinHistory.lastBlockIds(bitcoinHistory.bestBlock, length).map(id => bitcoinHistory.storage.getPoWDifficulty(Some(id)))
    val minmax = diffs.foldLeft((diffs(0).toInt, diffs(0).toInt)) { case ((min, max), e) => (scala.math.min(min, e.toInt), scala.math.max(max, e.toInt))}
    assert(minmax._2 / minmax._1 >= 16)
  }
}
