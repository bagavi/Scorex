package bitcoin

import commons.ExamplesCommonGenerators
import examples.commons._
import examples.bitcoin.blocks._
import examples.bitcoin.history.BitcoinSyncInfo
import examples.bitcoin.mining.BitcoinSettings
import examples.bitcoin.state.BitcoinBoxStoredState
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.block.Block._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state._
import scorex.core.transaction.wallet.WalletBox
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.Signature
import scorex.testkit.utils.{FileUtils, NoShrink}
import scorex.util.{ModifierId, bytesToId}

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.util.Random

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
trait HybridGenerators extends ExamplesCommonGenerators
  with StoreGenerators
  with HistoryGenerators
  with StateGenerators
  with ModifierGenerators
  with HybridTypes
  with NodeViewHolderGenerators
  with NodeViewSynchronizerGenerators
  with FileUtils
  with NoShrink {

  type ChangesGen = Gen[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]]

  val userConfigPath = "src/main/resources/settings.conf"
  val originalSettings = BitcoinSettings.read(Some(userConfigPath))
  override val settings = originalSettings.copy(mining = originalSettings.mining.copy(targetBlockDelay = 3.seconds, initialDifficulty = 1, blockGenerationDelay = 3.seconds, blockNetworkTransmissionDelay = 0.second))

  lazy val hybridSyncInfoGen: Gen[BitcoinSyncInfo] = for {
    answer <- Arbitrary.arbitrary[Boolean]
    pow <- modifierIdGen
    pows <- Gen.nonEmptyListOf(pow).map(_.take(BitcoinSyncInfo.MaxLastPowBlocks))
  } yield BitcoinSyncInfo(answer, pows)

  // Sign a random set of bytes
  lazy val signatureGen: Gen[Signature25519] = genBytes(Signature25519.SignatureSize)
    .map(g => Signature25519(Signature @@ g))

  lazy val blockIdGen: Gen[BlockId] = modifierIdGen

  lazy val blockIdsGen: Gen[Seq[BlockId]] = Gen.listOf(blockIdGen)

  lazy val nonEmptyBlockIdsGen: Gen[Seq[BlockId]] = Gen.nonEmptyListOf(blockIdGen)

  // Generate random pow header
  lazy val powHeaderGen: Gen[PowBlockHeader] = for {
    parentId: BlockId <- modifierIdGen
    timestamp: Long <- positiveLongGen
    nonce: Long <- positiveLongGen
    txs: Seq[SimpleBoxTransactionBitcoin] <- smallInt.flatMap(txNum => Gen.listOfN(txNum, simpleBoxTransactionBitcoinGen))
    proposition: PublicKey25519Proposition <- propositionGen
    fakeMinerId = bytesToId(Blake2b256("0"))
  } yield {
    val txsHash = if (txs.isEmpty) Array.fill(32)(0: Byte) else Blake2b256(PowBlockCompanion.txBytes(txs))
    new PowBlockHeader(parentId, timestamp, nonce, proposition, txs.length, txsHash, fakeMinerId)
  }
  // Generate random pow block
  lazy val powBlockGen: Gen[PowBlock] = for {
    parentId: BlockId <- modifierIdGen
    timestamp: Long <- positiveLongGen
    nonce: Long <- positiveLongGen
    txs: Seq[SimpleBoxTransactionBitcoin] <- smallInt.flatMap(txNum => Gen.listOfN(txNum, simpleBoxTransactionBitcoinGen))
    proposition: PublicKey25519Proposition <- propositionGen
    fakeMinerId = bytesToId(Blake2b256("0"))
  } yield {
    val txsHash = if (txs.isEmpty) Array.fill(32)(0: Byte) else Blake2b256(PowBlockCompanion.txBytes(txs))
    PowBlock.create(parentId, timestamp, nonce, proposition, txs, txsHash, fakeMinerId)
  }

  lazy val noncedBoxGen: Gen[PublicKey25519NoncedBox] = for {
    proposition <- propositionGen
    nonce <- nonceGen
    value <- valueGen
  } yield PublicKey25519NoncedBox(proposition, nonce, value)


  lazy val noncedBoxWithKeyGen: Gen[(PublicKey25519NoncedBox, PrivateKey25519)] = for {
    pair <- key25519Gen
    nonce <- nonceGen
    value <- valueGen
  } yield PublicKey25519NoncedBox(pair._2, nonce, value) -> pair._1


  lazy val noncedBoxListGen: Gen[List[PublicKey25519NoncedBox]] = for {
    count <- smallInt.map(_ + 5)
    boxList <- Gen.listOfN(count, noncedBoxGen)
  } yield boxList

  lazy val noncedBoxWithKeyListGen: Gen[List[(PublicKey25519NoncedBox, PrivateKey25519)]] = for {
    count <- smallInt.map(_ + 50)
    boxList <- Gen.listOfN(count, noncedBoxWithKeyGen)
  } yield boxList


  lazy val walletBoxGen: Gen[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]] = for {
    createdAt <- positiveLongGen
    txId <- modifierIdGen
    box: PublicKey25519NoncedBox <- noncedBoxGen
  } yield WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox](box, txId, createdAt)(PublicKey25519NoncedBoxSerializer)

  //Generator a transaction and the id
  val memPoolElementGen: Gen[(ModifierId, SimpleBoxTransactionBitcoin)] = for {
    id <- modifierIdGen
    transaction <- simpleBoxTransactionBitcoinGen
  } yield (id, transaction)
  //Generate a empty mem pool
  val emptyMemPoolGen: Gen[SimpleBoxTransactionBitcoinMemPool] = for {
    map <- Gen.buildableOfN[TrieMap[ModifierId, SimpleBoxTransactionBitcoin], (ModifierId, SimpleBoxTransactionBitcoin)](0, memPoolElementGen)
  } yield SimpleBoxTransactionBitcoinMemPool(map)

  //Generate BoxStateChanges
  def stateChangesGenerator(state: BitcoinBoxStoredState): ChangesGen = {
    val removals: List[BoxStateChangeOperation[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
      state.store.getAll().take(5).map(_._2).map(_.data)
        .map(PublicKey25519NoncedBoxSerializer.parseBytes).map(_.get).toList
        .map(b => Removal[PublicKey25519Proposition, PublicKey25519NoncedBox](b.id))

    noncedBoxListGen.map { boxesToAdd: List[PublicKey25519NoncedBox] =>
      val insertions: List[BoxStateChangeOperation[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
        boxesToAdd.map(b => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](b))
      val ops = scala.util.Random.shuffle(removals ++ insertions)
      BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](ops)
    }
  }

  //TODO Don't under this part
  def genValidTransactionPair(state: BitcoinBoxStoredState): Seq[SimpleBoxTransactionBitcoin] = {
    val keys = key25519Gen.apply(Gen.Parameters.default, Seed.random()).get
    val value = valueGen.apply(Gen.Parameters.default, Seed.random()).get

    val newBox: IndexedSeq[(PublicKey25519Proposition, Value)] = IndexedSeq((keys._2, value))
    val trx: SimpleBoxTransactionBitcoin = simpleBoxTransactionBitcoinGenCustomMakeBoxes(newBox).apply(Gen.Parameters.default, Seed.random()).get
    val useBox: IndexedSeq[(PrivateKey25519, Nonce)] = IndexedSeq((keys._1, trx.newBoxes.toVector(0).nonce))

    var trxnPair = Seq[SimpleBoxTransactionBitcoin]()
    trxnPair = trxnPair :+ trx
    trxnPair = trxnPair :+ simpleBoxTransactionBitcoinGenCustomUseBoxes(useBox).apply(Gen.Parameters.default, Seed.random()).get

    trxnPair
  }
  //Generate a pow block with state and transactions. but in my code, a pow block doesn't need state
  def semanticallyValidModifierWithCustomTransactions(state: BitcoinBoxStoredState,
                                                      transactions: Seq[SimpleBoxTransactionBitcoin]): PowBlock = {
    for {
      id <- modifierIdGen
      timestamp: Long <- positiveLongGen
      nonce: Long <- positiveLongGen
      txs: Seq[SimpleBoxTransactionBitcoin] = transactions
      proposition: PublicKey25519Proposition <- propositionGen
      fakeMinerId = bytesToId(Blake2b256("0"))
    } yield {
      val txsHash = if (txs.isEmpty) Array.fill(32)(0: Byte) else Blake2b256(PowBlockCompanion.txBytes(txs))
      PowBlock.create(id, timestamp, nonce, proposition, txs, txsHash, fakeMinerId)

    }
  }.apply(Gen.Parameters.default, Seed.random()).get

  //Generate a pow block with transactions in the mem pool
  def modifierWithTransactions(memoryPoolOpt: Option[SimpleBoxTransactionBitcoinMemPool],
                               customTransactionsOpt: Option[Seq[SimpleBoxTransactionBitcoin]]): PowBlock = {

    val (id, timestamp, nonce, proposition, minerId) = (for {
      id <- modifierIdGen
      timestamp: Long <- positiveLongGen
      nonce: Long <- positiveLongGen
      proposition: PublicKey25519Proposition <- propositionGen
      fakeMinerId = bytesToId(Blake2b256("0"))
    } yield (id, timestamp, nonce, proposition, fakeMinerId)).apply(Gen.Parameters.default, Seed.random()).get

    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val txs = memoryPoolOpt.map { memPool =>
      val toTake = Random.nextInt(memPool.size)
      Random.shuffle(memPool.take(memPool.size).toSeq).take(toTake)
    }.getOrElse(Seq()) ++ customTransactionsOpt.getOrElse(Seq()) match {
      case s if s.isEmpty => simpleBoxTransactionsBitcoinGen.sample.get
      case s => s
    }

    val txsHash = if (txs.isEmpty) Array.fill(32)(0: Byte) else Blake2b256(PowBlockCompanion.txBytes(txs))
    PowBlock.create(id, timestamp, nonce, proposition, txs, txsHash, minerId)
  }

  def privKey(value: Long): (PrivateKey25519, PublicKey25519Proposition) =
    PrivateKey25519Companion.generateKeys(("secret_" + value).getBytes)
}
