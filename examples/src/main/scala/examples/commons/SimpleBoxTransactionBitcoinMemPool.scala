package examples.commons

import scorex.core.transaction.MemoryPool
import scorex.util.ModifierId

import scala.collection.concurrent.TrieMap
import scala.util.{Success, Try}


case class SimpleBoxTransactionBitcoinMemPool(unconfirmed: TrieMap[ModifierId, SimpleBoxTransactionBitcoin])
  extends MemoryPool[SimpleBoxTransactionBitcoin, SimpleBoxTransactionBitcoinMemPool] {
  override type NVCT = SimpleBoxTransactionBitcoinMemPool

  //getters
  override def modifierById(id: ModifierId): Option[SimpleBoxTransactionBitcoin] = unconfirmed.get(id)

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleBoxTransactionBitcoin] = ids.flatMap(getById)

  //modifiers
  override def put(tx: SimpleBoxTransactionBitcoin): Try[SimpleBoxTransactionBitcoinMemPool] = Success {
    unconfirmed.put(tx.id, tx)
    this
  }

  //todo
  override def put(txs: Iterable[SimpleBoxTransactionBitcoin]): Try[SimpleBoxTransactionBitcoinMemPool] = Success(putWithoutCheck(txs))

  override def putWithoutCheck(txs: Iterable[SimpleBoxTransactionBitcoin]): SimpleBoxTransactionBitcoinMemPool = {
    txs.foreach(tx => unconfirmed.put(tx.id, tx))
    this
  }

  override def remove(tx: SimpleBoxTransactionBitcoin): SimpleBoxTransactionBitcoinMemPool = {
    unconfirmed.remove(tx.id)
    this
  }

  override def take(limit: Int): Iterable[SimpleBoxTransactionBitcoin] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(condition: SimpleBoxTransactionBitcoin => Boolean): SimpleBoxTransactionBitcoinMemPool = {
    unconfirmed.retain { (_, v) =>
      condition(v)
    }
    this
  }

  override def size: Int = unconfirmed.size
}


object SimpleBoxTransactionBitcoinMemPool {
  lazy val emptyPool: SimpleBoxTransactionBitcoinMemPool = SimpleBoxTransactionBitcoinMemPool(TrieMap())
}
