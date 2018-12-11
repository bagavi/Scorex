package examples.commons

import scorex.core.transaction.MemoryPool
import scorex.util.ModifierId

import scala.collection.concurrent.TrieMap
import scala.util.{Success, Try}


case class SimpleBoxTransactionPrismMemPool(unconfirmed: TrieMap[ModifierId, SimpleBoxTransactionPrism])
  extends MemoryPool[SimpleBoxTransactionPrism, SimpleBoxTransactionPrismMemPool] {
  override type NVCT = SimpleBoxTransactionPrismMemPool

  //getters
  override def modifierById(id: ModifierId): Option[SimpleBoxTransactionPrism] = unconfirmed.get(id)

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleBoxTransactionPrism] = ids.flatMap(getById)

  //modifiers
  override def put(tx: SimpleBoxTransactionPrism): Try[SimpleBoxTransactionPrismMemPool] = Success {
    unconfirmed.put(tx.id, tx)
    this
  }

  //todo
  override def put(txs: Iterable[SimpleBoxTransactionPrism]): Try[SimpleBoxTransactionPrismMemPool] = Success(putWithoutCheck(txs))

  override def putWithoutCheck(txs: Iterable[SimpleBoxTransactionPrism]): SimpleBoxTransactionPrismMemPool = {
    txs.foreach(tx => unconfirmed.put(tx.id, tx))
    this
  }

  override def remove(tx: SimpleBoxTransactionPrism): SimpleBoxTransactionPrismMemPool = {
    unconfirmed.remove(tx.id)
    this
  }

  override def take(limit: Int): Iterable[SimpleBoxTransactionPrism] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(condition: SimpleBoxTransactionPrism => Boolean): SimpleBoxTransactionPrismMemPool = {
    unconfirmed.retain { (_, v) =>
      condition(v)
    }
    this
  }

  override def size: Int = unconfirmed.size
}


object SimpleBoxTransactionPrismMemPool {
  lazy val emptyPool: SimpleBoxTransactionPrismMemPool = SimpleBoxTransactionPrismMemPool(TrieMap())
}
