package examples.bitcoin.validation

import examples.bitcoin.blocks.{BitcoinBlock, PowBlock}
import scorex.core.block.BlockValidator
import scorex.crypto.hash.{CryptographicHash, Digest}

import scala.util.Try

class SemanticBlockValidator(hash: CryptographicHash[_ <: Digest]) extends BlockValidator[BitcoinBlock] {

  def validate(block: BitcoinBlock): Try[Unit] = Try {
    block match {
      case powBlock: PowBlock =>
        require(powBlock.timestamp >= 0)

    }
  }
}
