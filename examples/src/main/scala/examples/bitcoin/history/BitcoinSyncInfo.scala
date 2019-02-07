package examples.bitcoin.history

import examples.bitcoin.blocks._
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.Serializer
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Try


/**
  * Stores up to 50 last PoW & Pos blocks
  * Thus maximum message size is about 100 * 32 ~= 3.2 KB
  * TODO answer is never used
  */
case class BitcoinSyncInfo(answer: Boolean,
                           lastPowBlockIds: Seq[ModifierId],
                         ) extends SyncInfo {

  import BitcoinSyncInfo.MaxLastPowBlocks

  require(lastPowBlockIds.size <= MaxLastPowBlocks)

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] =
    Seq(lastPowBlockIds.map(b => PowBlock.ModifierTypeId -> b)).flatten


  override type M = BitcoinSyncInfo

  override def serializer: Serializer[BitcoinSyncInfo] = BitcoinSyncInfoSerializer
}

object BitcoinSyncInfo {
  val MaxLastPowBlocks: Byte = 126 //don't make it more than 127 without changing serialization!
}

object BitcoinSyncInfoSerializer extends Serializer[BitcoinSyncInfo] {

  import BitcoinSyncInfo.MaxLastPowBlocks

  override def toBytes(obj: BitcoinSyncInfo): Array[Byte] =
    Array(
      if (obj.answer) 1: Byte else 0: Byte,
      obj.lastPowBlockIds.size.toByte
    ) ++ obj.lastPowBlockIds.foldLeft(Array[Byte]())((a, b) => a ++ idToBytes(b))

  override def parseBytes(bytes: Array[Byte]): Try[BitcoinSyncInfo] = Try {
    val answer: Boolean = if (bytes.head == 1.toByte) true else false
    val lastPowBlockIdsSize = bytes.slice(1, 2).head

    require(lastPowBlockIdsSize >= 0 && lastPowBlockIdsSize <= MaxLastPowBlocks)
    require(bytes.length == 2 + (lastPowBlockIdsSize + 0) * NodeViewModifier.ModifierIdSize)

    val lastPowBlockIds = bytes.slice(2, 2 + NodeViewModifier.ModifierIdSize * lastPowBlockIdsSize)
      .grouped(NodeViewModifier.ModifierIdSize).toSeq.map(id => bytesToId(id))

    BitcoinSyncInfo(answer, lastPowBlockIds)
  }
}

object BitcoinSyncInfoMessageSpec extends SyncInfoMessageSpec[BitcoinSyncInfo](BitcoinSyncInfoSerializer.parseBytes)
