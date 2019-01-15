package examples.prism1.validation

import examples.prism1.blocks.{HybridBlock, PowBlock}
import examples.prism1.history.HistoryStorage
import scorex.core.block.BlockValidator
import scorex.core.utils.ScorexEncoding

import scala.util.Try

class ParentBlockValidator(storage: HistoryStorage)
  extends BlockValidator[HybridBlock] with ScorexEncoding {

  def validate(block: HybridBlock): Try[Unit] = Try {
    block match {
      case powBlock: PowBlock => if (!storage.isGenesis(powBlock)) {
        //check PoW parent id ???
        require(storage.modifierById(powBlock.parentId).isDefined, s"Parent ${encoder.encodeId(powBlock.parentId)} missed")
      }
    }
  }

}
