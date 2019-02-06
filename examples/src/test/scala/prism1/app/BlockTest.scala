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

  private val TransactionsPerBlock: Int = 50

  private val getRequiredData: GetDataFromCurrentView[HybridHistory,
    HBoxStoredState,
    HBoxWallet,
    SimpleBoxTransactionPrismMemPool,
    PowMiningInfo] = {
    val f: CurrentView[HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionPrismMemPool] => PowMiningInfo = {
      view: CurrentView[HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionPrismMemPool] =>

        val difficulty = view.history.powDifficulty
        val bestPowBlock = view.history.bestPowBlock
        // Pick transactions from the view's mempool
        val txs = view.pool.take(TransactionsPerBlock).foldLeft(Seq[SimpleBoxTransactionPrism]()) { case (collected, tx) =>
          val txNonces = tx.from.map{case (_ , nonce ) => nonce}
          val collectedNonces = collected.flatMap(_.from).map{case (_ , nonce ) => nonce}
          if (view.state.validate(tx).isSuccess &&
            //ToDo: Vivek: I have edited this. To fix later.
            //            tx.boxIdsToOpen.forall(id => !collected.flatMap(_.boxIdsToOpen).contains(id))) collected :+ tx
            txNonces.forall(nonce => !collectedNonces.contains(nonce))) collected :+ tx
          else collected
        }

        log.info(s"${txs.size} out of ${view.pool.size} transactions added.")
        // TODO: fixme, What should we do if `view.vault.generateNewSecret().publicKeys` is empty?
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val pubkey = view.vault.publicKeys.headOption getOrElse view.vault.generateNewSecret().publicKeys.head
        PowMiningInfo(difficulty, bestPowBlock, pubkey, txs)
    }
    GetDataFromCurrentView[HybridHistory,
      HBoxStoredState,
      HBoxWallet,
      SimpleBoxTransactionPrismMemPool,
      PowMiningInfo](f)
  }

  private implicit val timeout = Timeout(5 seconds)

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

    {
      val future = app.nodeViewHolderRef ? getRequiredData
      val pmi: PowMiningInfo = Await.result(future, timeout.duration).asInstanceOf[PowMiningInfo]
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(PublicKey @@ encoder.decode("000000000000000a5177e290a0b1496751123eaef21992bcf5b20b9956bd1967").get)//a fake recipient.
      val fee: Long = 1L
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
      val tx: SimpleBoxTransactionPrism = SimpleBoxTransactionPrism.create(view._3, Seq((recipient, Value @@ 2L)), fee).get
      val pmiValid = pmi.copy(txs = Seq(tx))
      app.miner ! pmiValid
      Thread.sleep(2000)
      view = getNodeView(app.nodeViewHolderRef)
      assert(view._1.bestPowBlock.parentId == bestBlock.id)
      bestBlock = view._1.bestPowBlock
    }

    {
      val future = app.nodeViewHolderRef ? getRequiredData
      val pmi: PowMiningInfo = Await.result(future, timeout.duration).asInstanceOf[PowMiningInfo]
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(PublicKey @@ encoder.decode("000000000000000a5177e290a0b1496751123eaef21992bcf5b20b9956bd1967").get)//a fake recipient.
      val fee: Long = 1L
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
      val tx: SimpleBoxTransactionPrism = SimpleBoxTransactionPrism.create(view._3, Seq((recipient, Value @@ 2L)), fee).get
      val toInvalid: IndexedSeq[(PublicKey25519Proposition, Value)] =
        tx.to.map(p => (p._1, Value @@ (-p._2 - 1)))
      val txInvalid = tx.copy(to = toInvalid)
      val pmiInvalid = pmi.copy(txs = Seq(txInvalid))
      app.miner ! pmiInvalid
      Thread.sleep(2000)
      view = getNodeView(app.nodeViewHolderRef)
      assert(view._1.bestPowBlock.id == bestBlock.id)
    }


  }
}

