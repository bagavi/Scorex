package examples.prism1.validation

import examples.prism1.blocks.{HybridBlock, PowBlock}
import scorex.core.block.BlockValidator
import scorex.crypto.hash.{CryptographicHash, Digest}

import scala.util.Try

class SemanticBlockValidator(hash: CryptographicHash[_ <: Digest]) extends BlockValidator[HybridBlock] {

  def validate(block: HybridBlock): Try[Unit] = Try {
    block match {
      case powBlock: PowBlock =>
        require(powBlock.timestamp >= 0)

    }
  }
}
