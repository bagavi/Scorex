package bitcoin.serialization

import examples.commons.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer, SimpleBoxTransaction}
import examples.bitcoin.blocks.PowBlock
import examples.bitcoin.history.{BitcoinSyncInfo, BitcoinSyncInfoSerializer}
import bitcoin.BitcoinGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.wallet.{WalletBox, WalletBoxSerializer}

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
class SerializationTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BitcoinGenerators {

  property("WalletBox serialization") {
    val walletBoxSerializer =
      new WalletBoxSerializer[PublicKey25519Proposition, PublicKey25519NoncedBox](PublicKey25519NoncedBoxSerializer)
    forAll(walletBoxGen) { b: WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox] =>
      val parsed = walletBoxSerializer.parseBytes(walletBoxSerializer.toBytes(b)).get
      walletBoxSerializer.toBytes(parsed) shouldEqual walletBoxSerializer.toBytes(b)
    }
  }

  property("PublicKey25519NoncedBox serialization") {
    forAll(noncedBoxGen) { b: PublicKey25519NoncedBox =>
      val parsed = PublicKey25519NoncedBoxSerializer.parseBytes(PublicKey25519NoncedBoxSerializer.toBytes(b)).get
      parsed shouldEqual b
      PublicKey25519NoncedBoxSerializer.toBytes(parsed) shouldEqual PublicKey25519NoncedBoxSerializer.toBytes(b)
    }
  }

  property("PowBlock serialization") {
    forAll(powBlockGen) { b: PowBlock =>
      val parsed = b.serializer.parseBytes(b.bytes).get
      parsed.bytes shouldEqual b.bytes
    }
  }

  property("SimpleBoxTransaction serialization") {
    forAll(simpleBoxTransactionGen) { b: SimpleBoxTransaction =>
      val parsed = b.serializer.parseBytes(b.bytes).get
      parsed.bytes shouldEqual b.bytes
    }
  }

  property("BitcoinSyncInfo serialization") {
    forAll(bitcoinSyncInfoGen) { b: BitcoinSyncInfo =>
      val parsed = BitcoinSyncInfoSerializer.parseBytes(b.bytes).get
      parsed.bytes shouldEqual b.bytes
    }
  }

}
