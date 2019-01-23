package prism1.difficulty

import examples.prism1.blocks.{PowBlock, PowBlockCompanion}
import prism1.Generator._
import org.scalatest.PropSpec
import scorex.core.block.Block
import scorex.core.bytesToId
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Random, Success}

class DifficultyTest extends PropSpec{


  ignore("Should get an empty HybridHistory ") {
    val hybridHistory = hybridHistoryGenerator(3)
    assert(hybridHistory.height == 0)
  }

  property("Should get a Long HybridHistory") {
    val blockInterval: Int = 3
    var hybridHistory = hybridHistoryGenerator(blockInterval)
    val t0 = 1000000

    for (i <- 1 to 10) {
      val block: PowBlock = randomPowBlockGenerator(hybridHistory.bestPowId, t0 + blockInterval * i * 1000)
      hybridHistory.append(block) match {
        case Success((history, progressInfo)) =>
          hybridHistory = history
        case Failure(e) =>
          println(e)
      }
    }
    assert(hybridHistory.height == 10)
  }

  property("Should get a stable difficulty") {
    val blockInterval: Int = 3
    var hybridHistory = hybridHistoryGenerator(blockInterval)
    val t0 = 1000000
    val length = 100
    for (i <- 1 to length) {
      val block: PowBlock = randomPowBlockGenerator(hybridHistory.bestPowId, t0 + blockInterval * i * 1000 / 1000)
      hybridHistory.append(block) match {
        case Success((history, progressInfo)) =>
          hybridHistory = history
        case Failure(e) =>
          println(e)
      }
    }
    val diffs: Seq[BigInt] = hybridHistory.lastBlockIds(hybridHistory.bestBlock, length).map(id => hybridHistory.storage.getPoWDifficulty(Some(id)))
    println(diffs)
  }
}
