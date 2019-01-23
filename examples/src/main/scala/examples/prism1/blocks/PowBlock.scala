package examples.prism1.blocks

import com.google.common.primitives.{Bytes, Ints, Longs}
import examples.commons.{SimpleBoxTransactionPrism, SimpleBoxTransactionPrismCompanion}
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
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}

import scala.util.Try

class PowBlockHeader(
                      val parentId: BlockId,
                      val timestamp: Block.Timestamp,
                      val nonce: Long,
                      val generatorProposition: PublicKey25519Proposition,
                      val txsCount: Int,
                      val txsHash: Array[Byte],
                      val minerId: ModifierId
                      ) extends ScorexEncoding with ScorexLogging {

  import PowBlockHeader._

  val headerBytes: Array[Byte] =
    idToBytes(parentId) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(nonce) ++
      generatorProposition.pubKeyBytes ++
      Ints.toByteArray(txsCount) ++
      txsHash ++
      idToBytes(minerId)

  def correctWork(difficulty: BigInt, s: HybridMiningSettings): Boolean = {
    correctWorkDone(id, difficulty, s) || this.parentId == bytesToId(Array.fill(32)(0: Byte))
  }

  lazy val id: ModifierId = bytesToId(Blake2b256(headerBytes))

  override lazy val toString: String = s"PowBlockHeader(id: ${encoder.encodeId(id)})" +
    s"(parentId: ${encoder.encodeId(parentId)}, time: $timestamp, " +
    s"nonce: $nonce, txsCount: $txsCount, minerId: ${minerId})"
}

object PowBlockHeader extends ScorexLogging{


  //one 64 bit pointer, 2 long values and a pubkey.
  val PowHeaderSize = NodeViewModifier.ModifierIdSize + 8 * 2 + Curve25519.KeyLength + 4 + Blake2b256.DigestSize + NodeViewModifier.ModifierIdSize

  def parse(bytes: Array[Byte]): Try[PowBlockHeader] = Try {
    require(bytes.length == PowHeaderSize)
    val parentId = bytesToId(bytes.slice(0, 32))
    val timestamp = Longs.fromByteArray(bytes.slice(32, 40))
    val nonce = Longs.fromByteArray(bytes.slice(40, 48))
    val prop = PublicKey25519Proposition(PublicKey @@ bytes.slice(48, 80))
    val txsCount = Ints.fromByteArray(bytes.slice(80, 84))
    val txsHash = bytes.slice(84, 116)
    val minerId = bytesToId(bytes.slice(116,148))

    new PowBlockHeader(parentId, timestamp, nonce, prop, txsCount, txsHash, minerId)
  }

  def correctWorkDone(id: ModifierId, difficulty: BigInt, s: HybridMiningSettings): Boolean = {
    val target = s.MaxTarget / difficulty
    BigInt(1, idToBytes(id)) < target
  }
}

case class PowBlock(override val parentId: BlockId,
                    override val timestamp: Block.Timestamp,
                    override val nonce: Long,
                    override val generatorProposition: PublicKey25519Proposition,
                    val txs: Seq[SimpleBoxTransactionPrism],
                    override val txsHash: Array[Byte],
                    override val minerId: ModifierId
                   )
  extends PowBlockHeader(parentId, timestamp, nonce, generatorProposition, txs.length, txsHash, minerId )
    with HybridBlock with ScorexLogging {
  override type M = PowBlock

  override lazy val serializer = PowBlockCompanion

  override lazy val version: Version = 0: Byte

  override lazy val modifierTypeId: ModifierTypeId = PowBlock.ModifierTypeId

  lazy val txBytes = serializer.txBytes(txs)

//  override val txsHash = if (txs.isEmpty) Array.fill(32)(0: Byte) else Blake2b256(PowBlockCompanion.txBytes(txs))

  val txCounts: Int =  txs.length

  lazy val header = new PowBlockHeader(parentId, timestamp, nonce, generatorProposition, txCounts, txsHash, minerId)

  override lazy val toString: String = s"PoWBlock(${this.asJson.noSpaces})"

  //todo: coinbase transaction?
  override def transactions: Seq[SimpleBoxTransactionPrism] = txs
}

object PowBlockCompanion extends Serializer[PowBlock] with ScorexEncoding {

  def txBytes(transactions: Seq[SimpleBoxTransactionPrism]): Array[Byte] = {
    //For each transaction, encoding its length and the transaction .
    transactions.sortBy(t => encoder.encodeId(t.id)).foldLeft(Array[Byte]()) { (a, b) =>
      Bytes.concat(Ints.toByteArray(b.bytes.length), b.bytes, a)
    }
  }

  override def toBytes(modifier: PowBlock): Array[Byte] =
    modifier.headerBytes ++ modifier.txBytes

  override def parseBytes(bytes: Array[Byte]): Try[PowBlock] = {

    val headerBytes = bytes.slice(0, PowBlockHeader.PowHeaderSize)

    var position =  PowBlockHeader.PowHeaderSize // The transaction information starts here

    PowBlockHeader.parse(headerBytes).flatMap { header =>
      Try {
        val txs: Seq[SimpleBoxTransactionPrism] = (0 until header.txsCount) map { _ =>
          val l = Ints.fromByteArray(bytes.slice(position, position + 4)) // Contains the length of the transaction
          position = position + 4
          val tx = SimpleBoxTransactionPrismCompanion.parseBytes(bytes.slice(position, position + l)).get
          position = position + l
          tx
        }
        val prop = PublicKey25519PropositionSerializer.parseBytes(bytes.slice(48, 48 + Curve25519.KeyLength)).get
        PowBlock(
          header.parentId,
          header.timestamp,
          header.nonce,
          header.generatorProposition,
          txs,
          header.txsHash,
          header.minerId
        )
      }
    }
  }
}

object PowBlock extends ScorexEncoding with ScorexLogging {
  val ModifierTypeId: ModifierTypeId = scorex.core.ModifierTypeId @@ 3.toByte

  implicit val powBlockEncoder: Encoder[PowBlock] = (pb: PowBlock) => {
    Map(
      "id" -> encoder.encodeId(pb.id).asJson,
      "parentId" -> encoder.encodeId(pb.parentId).asJson,
      "timestamp" -> pb.timestamp.asJson,
      "nonce" -> pb.nonce.asJson,
      "txsCount" -> pb.txsCount.asJson,
      "txsHash" -> encoder.encode(pb.txsHash).asJson,
      "minerId" -> encoder.encodeId(pb.minerId).asJson
    ).asJson
  }
}
