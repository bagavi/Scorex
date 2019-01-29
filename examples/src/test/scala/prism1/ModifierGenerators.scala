package prism1

import commons.ExamplesCommonGenerators
import examples.commons.{Nonce, PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer, SimpleBoxTransactionPrism}
import examples.prism1.blocks.{HybridBlock, PowBlock, PowBlockCompanion}
import examples.prism1.history.HybridHistory
import examples.prism1.state.HBoxStoredState
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.versionToId
import scorex.crypto.hash.Blake2b256
import scorex.testkit.generators.{CoreGenerators, ModifierProducerTemplateItem, SynInvalid, Valid}
import scorex.util.{ModifierId, bytesToId}

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps",
                        "org.wartremover.warts.IsInstanceOf",
                        "org.wartremover.warts.OptionPartial"))
trait ModifierGenerators {
  this: HybridGenerators with CoreGenerators =>

  private val hf = Blake2b256

  val txCountGen: Gen[Int] = Gen.chooseNum(0, 40)
  val insPerTxCountGen: Gen[Int] = Gen.chooseNum(1, 10)
  val attachGen: Gen[Array[Byte]] = genBoundedBytes(0, 4096)

  val txGen: Gen[(Int, Int, Array[Byte])] = for {
    tx <- txCountGen
    in <- insPerTxCountGen
    at <- attachGen
  } yield (tx, in, at)

  /**
    * for each parentId, generate a valid child powblock containing txs in state
    * @param state
    * @param parentIds
    * @return
    */
  def validPowBlocks(state: HBoxStoredState, parentIds: Seq[ModifierId]): Seq[PowBlock] = {
    val count = parentIds.size
    require(count >= 1)

    val (txCount, insPerTx, attach) = txGen.sample.get

    assert(txCount >= 0 && txCount <= 40)
    assert(insPerTx >= 1 && insPerTx <= 10)

    def filterOutForgedBoxes(in: (ByteArrayWrapper, ByteArrayWrapper)): Boolean = {
      //current problem with unstable nodeviewholder spec is caused by coinbase block which always has value 1
      //so for now we just won't use it
      PublicKey25519NoncedBoxSerializer.parseBytes(in._2.data).map(_.value).getOrElse(0L) > 1L
    }

    val stateBoxes = state.store.getAll()
      .filter(filterOutForgedBoxes)
      .take(count * txCount * insPerTx + 1)
      .map { case (_, wrappedData) => PublicKey25519NoncedBoxSerializer.parseBytes(wrappedData.data).get }
      .toSeq

    assert(stateBoxes.size == count * txCount * insPerTx + 1)

    val txs = stateBoxes.tail.grouped(insPerTx).map { inputs =>
      val fee = 0
      val from = inputs.map(i => privKey(i.value)._1 -> i.nonce).toIndexedSeq
      val to = inputs.map(i => privKey(i.value)._2 -> i.value).toIndexedSeq
      SimpleBoxTransactionPrism(from, to, fee = fee, System.currentTimeMillis())
    }.toSeq

    txs.foreach {
      _.boxIdsToOpen.foreach { id => assert(state.closedBox(id).isDefined) }
    }

    val txsPerBlock = txs.size / count
    val txsGrouped =
      if(txsPerBlock == 0) Seq.fill(count)(Seq[SimpleBoxTransactionPrism]())
      else txs.grouped(txsPerBlock).toSeq

    assert(txsGrouped.size == count)

    val genBox: PublicKey25519NoncedBox = stateBoxes.head
    val proposition: PublicKey25519Proposition = genBox.proposition

    val fakeMinerId = bytesToId(Blake2b256("0"))

    txsGrouped.zip(parentIds).map{case (blockTxs, parentId) =>
      val txsHash = if (blockTxs.isEmpty) Array.fill(32)(0: Byte) else Blake2b256(PowBlockCompanion.txBytes(blockTxs))
      val nonce = positiveLongGen.sample.get
      PowBlock.create(parentId, System.currentTimeMillis(), nonce, proposition, blockTxs, txsHash, fakeMinerId)
    }
  }

  /**
    * Generate a semantically valid block whose parent is the latest version
    * @param state
    * @return
    */
  def semanticallyValidModifier(state: HBoxStoredState): PowBlock =
    validPowBlocks(state, Seq(versionToId(state.version))).head

  def syntacticallyValidModifier(curHistory: HybridHistory): HybridBlock =
    syntacticallyValidModifier(curHistory, Seq())

  def syntacticallyValidModifier(curHistory: HybridHistory, blocks: Seq[HybridBlock]): HybridBlock = {
    for {
      timestamp: Long <- positiveLongGen
      nonce: Long <- positiveLongGen
      txs: Seq[SimpleBoxTransactionPrism] <- smallInt.flatMap(txNum => Gen.listOfN(txNum, simpleBoxTransactionPrismGen))
      proposition: PublicKey25519Proposition <- propositionGen
      bestPowId = blocks.lastOption.map(_.id).getOrElse(curHistory.bestPowId)
      fakeMinerId = bytesToId(Blake2b256("0"))
    } yield {
      val txsHash = if (txs.isEmpty) Array.fill(32)(0: Byte) else Blake2b256(PowBlockCompanion.txBytes(txs))
      PowBlock.create(bestPowId, timestamp, nonce, proposition, txs, txsHash, fakeMinerId)
    }
  }.sample.get

  /**
    * generate several syntactically valid blocks appending to the history. transactions in them are random.
    * @param curHistory
    * @param count
    * @return
    */
  def syntacticallyValidModifiers(curHistory: HybridHistory, count: Int): Seq[HybridBlock] =
    (1 to count).foldLeft(Seq[HybridBlock]()) { case (blocks, _) =>
      blocks ++ Seq(syntacticallyValidModifier(curHistory, blocks))
    }

  private def makeSyntacticallyInvalid(mod: HybridBlock): HybridBlock = mod match {
    case pow: PowBlock => pow.copy(parentId = bytesToId(hf(pow.parentId)))
  }

  /**
    * Generate syntactically invalid block. (don't see it is called anywhere)
    * @param curHistory
    * @return
    */
  def syntacticallyInvalidModifier(curHistory: HybridHistory): HybridBlock =
    makeSyntacticallyInvalid(syntacticallyValidModifier(curHistory))

  /**
    * Generate semantically invalid block. (don't see it is called anywhere)
    * @param state
    * @return
    */
  def semanticallyInvalidModifier(state: HBoxStoredState): PowBlock = {
    val powBlock: PowBlock = semanticallyValidModifier(state)
    powBlock.transactions.lastOption.map { lastTx =>
      val modifiedFrom = (lastTx.from.head._1, Nonce @@ (lastTx.from.head._2 + 1)) +: lastTx.from.tail
      val modifiedLast = lastTx.copy(from = modifiedFrom)
      powBlock.copy(transactions = powBlock.transactions.dropRight(1) :+ modifiedLast)
    }.getOrElse {
      // if 0 txs in PowBlock, then it is semantically valid. how to invalid it?
      val tx = simpleBoxTransactionPrismGen.sample.get
      val modifiedFrom = (tx.from.head._1, Nonce @@ (tx.from.head._2 + 1)) +: tx.from.tail
      val modifiedLast = tx.copy(from = modifiedFrom)
      powBlock.copy(transactions = Seq(modifiedLast))
    }
  }

  /**
    * Generate a block that is both syntactically and semantically valid. That is, a block has transactions in state, and append to history
    * @param history
    * @param state
    * @return
    */
  def totallyValidModifier(history: HybridHistory, state: HBoxStoredState): HybridBlock =
    syntacticallyValidModifier(history) match {
      case powSyn: PowBlock =>
        val semBlock = semanticallyValidModifier(state)
        powSyn.copy(transactions = semBlock.transactions, txsHash = semBlock.txsHash)
    }

  /**
    * Generate a sequence of blocks that are both syntactically and semantically valid.
    * @param history
    * @param state
    * @return
    */
  def totallyValidModifiers(history: HT, state: ST, count: Int): Seq[HybridBlock] = {
    require(count >= 1)
    val mods = syntacticallyValidModifiers(history, count)

    val parentIds = mods.map(_.id)

    val powBlocks: Seq[HybridBlock] = validPowBlocks(state, parentIds)//semantically valid blocks

//    val validMods: Seq[HybridBlock] = mods

    val validMods: Seq[(HybridBlock, HybridBlock)] = mods zip powBlocks

    validMods.foldLeft((Seq[HybridBlock](), history.bestPowId)){case ((blocks, bestPw), b) =>
      b match {
        case (pwb1: PowBlock, pwb2: PowBlock) =>
          val newPwb = pwb1.copy(parentId = bestPw, transactions = pwb2.transactions, txsHash = pwb2.txsHash)
          (blocks ++ Seq(newPwb), newPwb.id)
        case _ => (blocks, bestPw)
      }
    }._1
  }.ensuring{blocks =>
    lazy val head = blocks.head
    lazy val headLinksValid = head match {
      case pwb: PowBlock =>
        history.bestPowId == pwb.parentId
    }
    headLinksValid && history.applicableTry(head).isSuccess
  }

  def customModifiers(history: HT,
                      state: ST,
                      template: Seq[ModifierProducerTemplateItem]): Seq[PM] =
    template.zip(totallyValidModifiers(history, state, template.length))
      .map { case (templateItem, mod) =>
        templateItem match {
          case Valid => mod
          case SynInvalid => makeSyntacticallyInvalid(mod)
        }
      }
}
