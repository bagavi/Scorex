package bitcoin

import examples.bitcoin.blocks.PowBlock
import examples.bitcoin.history.{HistoryStorage, BitcoinHistory}
import examples.bitcoin.mining.BitcoinSettings
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.PublicKey
import scorex.util.bytesToId

import scala.concurrent.ExecutionContext.Implicits.global

trait HistoryGenerators {
  this: StoreGenerators =>

  def settings: BitcoinSettings

  private val historyTimestamp = 1478164225796L
  private val historyNonce = -308545845552064644L
  private val historyBrothersCount = 0
  private val historyBrothersHash = Array.fill(32)(0: Byte)
  private val historyBrothers = Seq.empty
  private val historyProposition = PublicKey25519Proposition(PublicKey @@ scorex.utils.Random.randomBytes(32))
  private val txs = Seq()
  private val txsHash = Array.fill(32)(0: Byte)
  private val minerId = bytesToId(Blake2b256("0")) // A fake Id

  private lazy val genesisBlock = PowBlock(
    settings.mining.GenesisParentId,
    historyTimestamp,
    historyNonce,
    historyProposition,
    txs,
    txsHash,
    minerId
    )

  /*
  Takes an empty generated database and adds a genesis block
   */
  val historyGen: Gen[BitcoinHistory] = lsmStoreGen.map { blockStorage =>
    val storage = new HistoryStorage(blockStorage, settings.mining)
    //we don't care about validation here
    val validators = Seq()
    new BitcoinHistory(storage, settings.mining, validators, None, new NetworkTimeProvider(settings.scorexSettings.ntp))
      .append(genesisBlock).get._1
      .ensuring(_.modifierById(genesisBlock.id).isDefined)
  }
}
