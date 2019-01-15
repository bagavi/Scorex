package examples.prism1.validation

import examples.prism1.blocks.{HybridBlock, PowBlock}
import examples.prism1.history.HistoryStorage
import examples.prism1.mining.{HybridMiningSettings}
import scorex.core.block.BlockValidator
import scorex.core.utils.ScorexEncoding

import scala.util.Try

class DifficultyBlockValidator(settings: HybridMiningSettings, storage: HistoryStorage)
  extends BlockValidator[HybridBlock] with ScorexEncoding {

  def validate(block: HybridBlock): Try[Unit] = block match {
    case b: PowBlock => checkPoWConsensusRules(b)
  }

  //PoW consensus rules checks, work/references
  //throws exception if anything wrong
  private def checkPoWConsensusRules(powBlock: PowBlock): Try[Unit] = Try {
    val powDifficulty = storage.getPoWDifficulty(Some(powBlock.prevPosId))
    //check work
    require(powBlock.correctWork(powDifficulty, settings),
      s"Work done is incorrect for block ${encoder.encodeId(powBlock.id)} and difficulty $powDifficulty")

    //some brothers work
    require(powBlock.brothers.forall(_.correctWork(powDifficulty, settings)))

  }

}
