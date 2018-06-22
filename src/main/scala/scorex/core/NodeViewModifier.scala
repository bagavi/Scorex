package scorex.core

import com.typesafe.config.ConfigFactory
import scorex.core.serialization.BytesSerializable
import scorex.core.transaction.Transaction
import scorex.core.utils.ScorexEncoding

import scala.util.Try

sealed trait NodeViewModifier extends BytesSerializable with ScorexEncoding {
  self =>

  val modifierTypeId: ModifierTypeId

  //todo: check statically or dynamically output size
  def id: ModifierId

  def encodedId: String = encoder.encode(id)

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: NodeViewModifier => (that.id sameElements id) && (that.modifierTypeId == modifierTypeId)
    case _ => false
  }
}

trait EphemerealNodeViewModifier extends NodeViewModifier

/**
  * It is supposed that all the modifiers (offchain transactions, blocks, blockheaders etc)
  * have identifiers of the some length fixed with the ModifierIdSize constant
  */
object NodeViewModifier {
  private val DefaultIdSize: Byte = 32 // in bytes

  val ModifierIdSize: Int = Try(ConfigFactory.load().getConfig("app").getInt("modifierIdSize")).getOrElse(DefaultIdSize)
}


trait PersistentNodeViewModifier extends NodeViewModifier {
  // TODO remove. parentId is not nessesary for persistent modifiers, e.g. most of persistent modifiers in Ergo
  // TODO (BlockTransactions, ADProofs, PoPoWProofs, ...) does not have parent id
  def parentId: ModifierId
}


trait TransactionsCarryingPersistentNodeViewModifier[TX <: Transaction]
  extends PersistentNodeViewModifier {

  def transactions: Seq[TX]
}
