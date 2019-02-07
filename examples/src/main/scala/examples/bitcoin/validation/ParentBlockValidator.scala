package examples.bitcoin.validation

import examples.bitcoin.blocks.{BitcoinBlock, PowBlock}
import examples.bitcoin.history.HistoryStorage
import scorex.core.block.BlockValidator
import scorex.core.utils.ScorexEncoding

import scala.util.Try

class ParentBlockValidator(storage: HistoryStorage)
  extends BlockValidator[BitcoinBlock] with ScorexEncoding {

  def validate(block: BitcoinBlock): Try[Unit] = Try {
    block match {
      case powBlock: PowBlock => if (!storage.isGenesis(powBlock)) {
        //check PoW parent id ???
        require(storage.modifierById(powBlock.parentId).isDefined, s"Parent ${encoder.encodeId(powBlock.parentId)} missed")
      }
    }
  }

}
