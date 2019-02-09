package examples.bitcoin.validation

import examples.bitcoin.blocks.{BitcoinBlock, PowBlock, PowBlockCompanion}
import scorex.core.block.BlockValidator
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{CryptographicHash, Digest}

import scala.util.{Failure, Success, Try}

class SemanticBlockValidator(hash: CryptographicHash[_ <: Digest]) extends BlockValidator[BitcoinBlock] {

  def validate(block: BitcoinBlock): Try[Unit] = Try {
    block match {
      case powBlock: PowBlock =>
        require(powBlock.timestamp >= 0)
        require( (powBlock.txsHash sameElements Array.fill(32)(0: Byte)) || //this is special txsHash for genesis
          (powBlock.txsHash sameElements hash(PowBlockCompanion.txBytes(powBlock.transactions))))
        //All tx must be valid
        require(powBlock.transactions.forall(tx => tx.semanticValidity.isSuccess))
        //No box (coin) should be double spent by two txs
        powBlock.transactions.map(_.boxIdsToOpen).foldLeft(IndexedSeq[ADKey]()) {
          (partialSet, ids) =>
            require(ids.forall(id => !partialSet.exists(_ sameElements id)))
            partialSet ++ ids
        }
    }
  }
}
