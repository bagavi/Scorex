package examples.bitcoin.validation

import examples.bitcoin.blocks.{BitcoinBlock, PowBlock}
import examples.bitcoin.history.HistoryStorage
import examples.bitcoin.mining.BitcoinMiningSettings
import scorex.core.block.BlockValidator
import scorex.core.utils.ScorexEncoding
import scorex.util.ScorexLogging

import scala.util.Try

class DifficultyBlockValidator(settings: BitcoinMiningSettings, storage: HistoryStorage)
  extends BlockValidator[BitcoinBlock] with ScorexEncoding with ScorexLogging{

  def validate(block: BitcoinBlock): Try[Unit] = block match {
    case b: PowBlock => checkPoWConsensusRules(b)
  }

  //PoW consensus rules checks, work/references
  //throws exception if anything wrong
  private def checkPoWConsensusRules(powBlock: PowBlock): Try[Unit] = Try {
    val powDifficulty = storage.getPoWDifficulty(None)
    //check work

    require(powBlock.correctWork(powDifficulty, settings),
      s"Work done is incorrect for block ${encoder.encodeId(powBlock.id)} and difficulty $powDifficulty")
  }

}
