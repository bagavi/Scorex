package examples.bitcoin.validation

import examples.bitcoin.blocks.{BitcoinBlock, PowBlock, PowBlockCompanion}
import scorex.core.block.BlockValidator
import scorex.crypto.hash.{CryptographicHash, Digest}

import scala.util.Try

class SemanticBlockValidator(hash: CryptographicHash[_ <: Digest]) extends BlockValidator[BitcoinBlock] {

  def validate(block: BitcoinBlock): Try[Unit] = Try {
    block match {
      case powBlock: PowBlock =>
        require(powBlock.timestamp >= 0)
        require( (powBlock.txsHash sameElements Array.fill(32)(0: Byte)) || //this is special txsHash for genesis
          (powBlock.txsHash sameElements hash(PowBlockCompanion.txBytes(powBlock.transactions))))
        require(powBlock.transactions.forall(tx => tx.semanticValidity.isSuccess))
    }
  }
}
