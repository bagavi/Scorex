package bitcoin.transaction

import examples.commons.SimpleBoxTransactionBitcoin
import bitcoin.BitcoinGenerators
import io.iohk.iodb.ByteArrayWrapper
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.state.PrivateKey25519Companion


class TransactionSuite extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BitcoinGenerators {

  property("SimpleBoxTransaction preservers inputs ids") {
    forAll(noncedBoxWithKeyListGen, noncedBoxWithKeyListGen) { case (boxesWithKeysIn, boxesWithKeysOut) =>

      val inputs = boxesWithKeysIn.map { case (box, k) => k -> box.nonce }.toIndexedSeq

      boxesWithKeysIn.foreach { case (box, k) =>
        PrivateKey25519Companion.owns(k, box) shouldBe true
      }

      val boxIds = boxesWithKeysIn.map(_._1.id).map(ByteArrayWrapper.apply)

      val to = boxesWithKeysOut.map { case (box, _) =>
        box.proposition -> box.value
      }.toIndexedSeq

      val tx: SimpleBoxTransactionBitcoin = SimpleBoxTransactionBitcoin(inputs, to, 0, 0)

      val outKeys = boxesWithKeysOut.map(_._2).map(_.publicKeyBytes).map(ByteArrayWrapper.apply)

      tx.newBoxes.foreach { newBox =>
        outKeys.contains(ByteArrayWrapper(newBox.proposition.pubKeyBytes)) shouldBe true
      }

      tx.boxIdsToOpen.map(ByteArrayWrapper.apply).forall { bid =>
        boxIds.contains(bid)
      } shouldBe true
    }
  }

}
