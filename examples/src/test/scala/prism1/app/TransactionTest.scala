package prism1.app

import examples.commons.{SimpleBoxTransactionPrism, Value}
import examples.prism1.PrismV1App
import examples.prism1.mining.PowMiner.ReceivableMessages.{StartMining, StopMining}
import org.scalatest.PropSpec
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexEncoding
import scorex.crypto.signatures.PublicKey

import scala.sys.process._
import scala.util.{Failure, Success, Try}


class TransactionTest extends PropSpec with ScorexEncoding{
  /**
    * Please run ConfigTestGenerator.sh first
    */
  import RunMainTest._

  ignore("Send StartMining/StopMining to miner, it should start/stop mining") {
    "src/main/resources/testbench/ConfigTestGenerator.sh topology.txt" !

    val app = new PrismV1App("src/main/resources/testbench/testsettings1.conf")
    app.run()
    assert(getNodeView(app.nodeViewHolderRef)._1.height == 1)
    Thread.sleep(1000)
    app.miner ! StartMining
    Thread.sleep(20000)
    app.miner ! StopMining
    Thread.sleep(5000)
    val h1 = getNodeView(app.nodeViewHolderRef)._1.height
    Thread.sleep(5000)
    val h2 = getNodeView(app.nodeViewHolderRef)._1.height
    assert(h1 == h2)

  }

  ignore("See a tx get confirmed. Steps: 1) let it mine for some time 2) get the balance 3) transfer all the balance to others 4) check the tx is in chain") {
    "src/main/resources/testbench/ConfigTestGenerator.sh topology.txt" !

    val app = new PrismV1App("src/main/resources/testbench/testsettings1.conf")
    app.run()
    Thread.sleep(1000)
    app.miner ! StartMining
    Thread.sleep(10000)
    app.miner ! StopMining
    Thread.sleep(5000)
    var view = getNodeView(app.nodeViewHolderRef)
    val balance = view._3.boxes().map(_.box).map(_.value.toLong).sum
    assert(balance > 0)//it mines some blocks, so it should have coins
    assert(view._4.size == 0)//it doesn't generate/receive tx, so mem pool should be empty
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
    val recipient: PublicKey25519Proposition = PublicKey25519Proposition(PublicKey @@ encoder.decode("000000000000000a5177e290a0b1496751123eaef21992bcf5b20b9956bd1967").get)// a fake recipient
    val fee: Long = 1L
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
    val tx: SimpleBoxTransactionPrism = SimpleBoxTransactionPrism.create(view._3, Seq((recipient, Value @@ (balance - fee))), fee).get
    app.nodeViewHolderRef ! LocallyGeneratedTransaction[SimpleBoxTransactionPrism](tx)
    Thread.sleep(500)
    view = getNodeView(app.nodeViewHolderRef)
    assert(view._4.contains(tx))//mem pool should contain this tx
    app.miner ! StartMining
    Thread.sleep(10000)
    view = getNodeView(app.nodeViewHolderRef)
    assert(!view._4.contains(tx))//mem pool should not contain this tx
    assert(view._1.lastPowBlocks(Int.MaxValue, view._1.bestPowBlock).exists(b => b.transactions.map(_.id).contains(tx.id)))//a block in chain should contain this tx
  }

  property("See two tx conflicts. Steps: 1) get a tx 2) copy it with another timestamp 3)add them 4) check the second got rejected") {
    "src/main/resources/testbench/ConfigTestGenerator.sh topology.txt" !

    val app = new PrismV1App("src/main/resources/testbench/testsettings1.conf")
    app.run()
    Thread.sleep(1000)
    app.miner ! StartMining
    Thread.sleep(10000)
    app.miner ! StopMining
    Thread.sleep(5000)
    var view = getNodeView(app.nodeViewHolderRef)
    val balance = view._3.boxes().map(_.box).map(_.value.toLong).sum
    assert(balance > 0)//it mines some blocks, so it should have coins
    assert(view._4.size == 0)//it doesn't generate/receive tx, so mem pool should be empty
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
    val recipient: PublicKey25519Proposition = PublicKey25519Proposition(PublicKey @@ encoder.decode("000000000000000a5177e290a0b1496751123eaef21992bcf5b20b9956bd1967").get)// a fake recipient
    val fee: Long = 1L
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
    val tx: SimpleBoxTransactionPrism = SimpleBoxTransactionPrism.create(view._3, Seq((recipient, Value @@ (balance - fee))), fee).get
    val tx2 = tx.copy(timestamp = System.currentTimeMillis())
    app.nodeViewHolderRef ! LocallyGeneratedTransaction[SimpleBoxTransactionPrism](tx)
    app.nodeViewHolderRef ! LocallyGeneratedTransaction[SimpleBoxTransactionPrism](tx2)
    Thread.sleep(500)
    view = getNodeView(app.nodeViewHolderRef)
    assert(view._4.contains(tx))//mem pool should contain this tx
    assert(!view._4.contains(tx2))//mem pool should not contain this tx2
  }
}

