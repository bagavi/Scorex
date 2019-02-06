package prism1.app

import akka.pattern.ask
import akka.util.Timeout
import examples.commons.{SimpleBoxTransactionPrism, SimpleBoxTransactionPrismMemPool, Value}
import examples.prism1.PrismV1App
import examples.prism1.blocks.{HybridBlock, PowBlock}
import examples.prism1.history.HybridHistory
import examples.prism1.mining.{PowMiner, PowMinerRef}
import examples.prism1.mining.PowMiner.ReceivableMessages.{PowMiningInfo, StartMining, StopMining}
import examples.prism1.state.HBoxStoredState
import examples.prism1.wallet.HBoxWallet
import org.scalatest.PropSpec
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexEncoding
import scorex.crypto.signatures.PublicKey
import scorex.util.ScorexLogging

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.sys.process._


class BlockTest extends PropSpec with ScorexEncoding with ScorexLogging{
  //private implicit val timeout = Timeout(5 seconds)

  import RunMainTest._

  property("See invalid blocks get rejected. Steps: 1) get a tx 2) copy it with another timestamp 3)add them 4) check the second got rejected") {
    "src/main/resources/testbench/ConfigTestGenerator.sh topology.txt" !

    val app = new PrismV1App("src/main/resources/testbench/testsettings1.conf")
    app.run()
    Thread.sleep(1000)
    app.miner ! StartMining
    Thread.sleep(2000)
    app.miner ! StopMining
    Thread.sleep(1000)

    var view = getNodeView(app.nodeViewHolderRef)
    var bestBlock: PowBlock = view._1.bestPowBlock
    var balance = view._3.boxes().map(_.box).map(_.value.toLong).sum

    {
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(PublicKey @@ encoder.decode("000000000000000a5177e290a0b1496751123eaef21992bcf5b20b9956bd1967").get)//a fake recipient.
      val fee: Long = 1L
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
      val tx: SimpleBoxTransactionPrism = SimpleBoxTransactionPrism.create(view._3, Seq((recipient, Value @@ 2L)), fee).get
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
      val pubkey = view._3.publicKeys.headOption getOrElse view._3.generateNewSecret().publicKeys.head
      val pmiValid = PowMiningInfo(view._1.powDifficulty, view._1.bestPowBlock, pubkey ,Seq(tx))
      app.miner ! pmiValid
      Thread.sleep(2000)
      view = getNodeView(app.nodeViewHolderRef)
      assert(view._1.bestPowBlock.parentId == bestBlock.id)
      assert(balance + 83 - 2 - 1 == view._3.boxes().map(_.box).map(_.value.toLong).sum)
      balance = view._3.boxes().map(_.box).map(_.value.toLong).sum
      bestBlock = view._1.bestPowBlock
    }

    {
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(PublicKey @@ encoder.decode("000000000000000a5177e290a0b1496751123eaef21992bcf5b20b9956bd1967").get)//a fake recipient.
      val fee: Long = 1L
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
      val tx: SimpleBoxTransactionPrism = SimpleBoxTransactionPrism.create(view._3, Seq((recipient, Value @@ 2L)), fee).get
      val toInvalid: IndexedSeq[(PublicKey25519Proposition, Value)] =
        tx.to.map(p => (p._1, Value @@ (-p._2 - 1)))
      val txInvalid = tx.copy(to = toInvalid)
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
      val pubkey = view._3.publicKeys.headOption getOrElse view._3.generateNewSecret().publicKeys.head
      val pmiInvalid = PowMiningInfo(view._1.powDifficulty, view._1.bestPowBlock, pubkey ,Seq(txInvalid))
      app.miner ! pmiInvalid
      Thread.sleep(2000)
      view = getNodeView(app.nodeViewHolderRef)
      assert(view._1.bestPowBlock.parentId == bestBlock.id)
      assert(balance == view._3.boxes().map(_.box).map(_.value.toLong).sum)
      balance = view._3.boxes().map(_.box).map(_.value.toLong).sum
      bestBlock = view._1.bestPowBlock
    }

    {
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(PublicKey @@ encoder.decode("000000000000000a5177e290a0b1496751123eaef21992bcf5b20b9956bd1967").get)//a fake recipient.
      val fee: Long = 1L
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
      val tx: SimpleBoxTransactionPrism = SimpleBoxTransactionPrism.create(view._3, Seq((recipient, Value @@ 2L)), fee).get
      val txInvalid = tx.copy(fee = 2L)
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
      val pubkey = view._3.publicKeys.headOption getOrElse view._3.generateNewSecret().publicKeys.head
      val pmiInvalid = PowMiningInfo(view._1.powDifficulty, view._1.bestPowBlock, pubkey ,Seq(txInvalid))
      app.miner ! pmiInvalid
      Thread.sleep(2000)
      view = getNodeView(app.nodeViewHolderRef)
      assert(view._1.bestPowBlock.parentId == bestBlock.id)
      assert(balance == view._3.boxes().map(_.box).map(_.value.toLong).sum)
      balance = view._3.boxes().map(_.box).map(_.value.toLong).sum
      bestBlock = view._1.bestPowBlock
    }

  }
}

