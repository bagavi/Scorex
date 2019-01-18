package examples.prism1.blocks

import com.google.common.primitives.{Ints, Longs}
import examples.commons.SimpleBoxTransactionPrism
import examples.prism1.mining.HybridMiningSettings
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PublicKey}
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Try

class PowBlockHeader(
                      val parentId: BlockId,
                      val timestamp: Block.Timestamp,
                      val nonce: Long,
                      val generatorProposition: PublicKey25519Proposition) extends ScorexEncoding {


  import PowBlockHeader._

  lazy val headerBytes: Array[Byte] =
    idToBytes(parentId) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(nonce) ++
      generatorProposition.pubKeyBytes

  def correctWork(difficulty: BigInt, s: HybridMiningSettings): Boolean = correctWorkDone(id, difficulty, s)

  lazy val id: ModifierId = bytesToId(Blake2b256(headerBytes))

  override lazy val toString: String = s"PowBlockHeader(id: ${encoder.encodeId(id)})" +
    s"(parentId: ${encoder.encodeId(parentId)}, time: $timestamp, " + s"nonce: $nonce)"
}

object PowBlockHeader {
  //one 64 bit pointer, 2 long values and a pubkey.
  val PowHeaderSize = NodeViewModifier.ModifierIdSize + 8 * 2 + Curve25519.KeyLength

  def parse(bytes: Array[Byte]): Try[PowBlockHeader] = Try {
    require(bytes.length == PowHeaderSize)
    val parentId = bytesToId(bytes.slice(0, 32))
    val timestamp = Longs.fromByteArray(bytes.slice(32, 40))
    val nonce = Longs.fromByteArray(bytes.slice(40, 48))
    val prop = PublicKey25519Proposition(PublicKey @@ bytes.slice(48, 80))

    new PowBlockHeader(parentId, timestamp, nonce, prop)
  }

  def correctWorkDone(id: ModifierId, difficulty: BigInt, s: HybridMiningSettings): Boolean = {
    val target = s.MaxTarget / difficulty
    BigInt(1, idToBytes(id)) < target
  }
}

case class PowBlock(override val parentId: BlockId,
                    override val timestamp: Block.Timestamp,
                    override val nonce: Long,
                    override val generatorProposition: PublicKey25519Proposition)
  extends PowBlockHeader(parentId, timestamp, nonce, generatorProposition)
    with HybridBlock {

  override type M = PowBlock

  override lazy val serializer = PowBlockCompanion

  override lazy val version: Version = 0: Byte

  override lazy val modifierTypeId: ModifierTypeId = PowBlock.ModifierTypeId


  lazy val header = new PowBlockHeader(parentId, timestamp, nonce, generatorProposition)


  override lazy val toString: String = s"PoWBlock(${this.asJson.noSpaces})"

  //todo: coinbase transaction?
  override def transactions: Seq[SimpleBoxTransactionPrism] = Seq()
}

object PowBlockCompanion extends Serializer[PowBlock] {

  //
  def brotherBytes(brothers: Seq[PowBlockHeader]): Array[Byte] = brothers.foldLeft(Array[Byte]()) { case (ba, b) =>
    ba ++ b.headerBytes
  }

  override def toBytes(modifier: PowBlock): Array[Byte] =
    modifier.headerBytes ++ modifier.generatorProposition.bytes

  override def parseBytes(bytes: Array[Byte]): Try[PowBlock] = {

    val headerBytes = bytes.slice(0, PowBlockHeader.PowHeaderSize)
    /*
      Loop through the PoWBlock header to extract brother block hashes
     */
    PowBlockHeader.parse(headerBytes).flatMap { header =>
      Try {
        val prop = PublicKey25519PropositionSerializer.parseBytes(bytes.slice(48, 48 + Curve25519.KeyLength)).get
        PowBlock(
          header.parentId,
          header.timestamp,
          header.nonce,
          prop,
        )
      }
    }
  }
}

object PowBlock extends ScorexEncoding {
  val ModifierTypeId: ModifierTypeId = scorex.core.ModifierTypeId @@ 3.toByte

  implicit val powBlockEncoder: Encoder[PowBlock] = (pb: PowBlock) => {
    Map(
      "id" -> encoder.encodeId(pb.id).asJson,
      "timestamp" -> pb.timestamp.asJson,
      "nonce" -> pb.nonce.asJson,
    ).asJson
  }
}
