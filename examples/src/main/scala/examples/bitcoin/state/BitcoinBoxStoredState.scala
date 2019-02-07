package examples.bitcoin.state

import java.io.File

import com.google.common.primitives.Longs
import examples.commons._
import examples.bitcoin.blocks.{BitcoinBlock, PowBlock}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core._
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{BoxStateChangeOperation, BoxStateChanges, Insertion, Removal}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds._
import scorex.mid.state.BoxMinimalState
import scorex.util.ScorexLogging

import scala.util.{Failure, Success, Try}


case class BitcoinBoxStoredState(store: LSMStore, override val version: VersionTag) extends
  BoxMinimalState[PublicKey25519Proposition,
    PublicKey25519NoncedBox,
    SimpleBoxTransactionBitcoin,
    BitcoinBlock,
    BitcoinBoxStoredState] with ScorexLogging with ScorexEncoding {

  require(store.lastVersionID.map(w => bytesToVersion(w.data)).getOrElse(version) == version,
    s"${encoder.encodeVersion(store.lastVersionID.map(w => bytesToVersion(w.data)).getOrElse(version))}" +
      s" != ${encoder.encodeVersion(version)}")

  override type NVCT = BitcoinBoxStoredState
  type HPMOD = BitcoinBlock

  override def semanticValidity(tx: SimpleBoxTransactionBitcoin): Try[Unit] = BitcoinBoxStoredState.semanticValidity(tx)

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] =
    store.get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(PublicKey25519NoncedBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  //there's no easy way to know boxes associated with a proposition without an additional index
  override def boxesOf(proposition: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] = ???

  override def changes(mod: HPMOD): Try[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
    BitcoinBoxStoredState.changes(mod)

  //Validate transactions in block and generator box
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  override def validate(mod: HPMOD): Try[Unit] = Try {
    mod match {
      case pwb: PowBlock =>
        //Gerui: original comment which is not true: coinbase transaction is generated implicitly when block is applied to state, no validation needed
        require((pwb.parentId == version) , s"Incorrect state version: ${encoder.encodeVersion(version)} " +
          s"found, (${encoder.encodeId(pwb.parentId)} expected" )
        pwb.transactions.foreach(tx => validate(tx).get)

    }
  }.recoverWith{case t =>
    log.warn(s"Not valid modifier ${mod.encodedId}", t)
    Failure(t)
  }

  override def applyChanges(changes: BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox],
                            newVersion: VersionTag): Try[BitcoinBoxStoredState] = Try {
    val boxIdsToRemove = changes.toRemove.map(_.boxId).map(ByteArrayWrapper.apply)
      .ensuring(_.forall(i => closedBox(i.data).isDefined) || store.lastVersionID.isEmpty)
    val boxesToAdd = changes.toAppend.map(_.box).map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

    log.trace(s"Update BitcoinBoxStoredState from version $lastVersionString to version ${encoder.encodeVersion(newVersion)}. " +
      s"Removing boxes with ids ${boxIdsToRemove.map(b => encoder.encode(b.data))}, " +
      s"adding boxes ${boxesToAdd.map(b => encoder.encode(b._1.data))}")
    store.update(versionToBAW(newVersion), boxIdsToRemove, boxesToAdd)
    BitcoinBoxStoredState(store, newVersion)
      .ensuring(st => boxIdsToRemove.forall(box => st.closedBox(box.data).isEmpty), s"Removed box is still in state")
  } ensuring { r => r.toOption.forall(_.version == newVersion )}

  override def maxRollbackDepth: Int = store.keepVersions

  override def rollbackTo(version: VersionTag): Try[BitcoinBoxStoredState] = Try {
    if (store.lastVersionID.exists(w => bytesToVersion(w.data) == version)) {
      this
    } else {
      log.info(s"Rollback BitcoinBoxStoredState to ${encoder.encodeVersion(version)} from version $lastVersionString")
      store.rollback(versionToBAW(version))
      new BitcoinBoxStoredState(store, version)
    }
  }.recoverWith{case e =>
    log.error("Cant' do rollback: ", e)
    Failure[BitcoinBoxStoredState](e): Try[BitcoinBoxStoredState]
  }

  private def lastVersionString = store.lastVersionID.map(v => encoder.encode(v.data)).getOrElse("None")

}

object BitcoinBoxStoredState {
  def semanticValidity(tx: SimpleBoxTransactionBitcoin): Try[Unit] = tx.semanticValidity

  def changes(mod: BitcoinBlock): Try[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
    mod match {
      case pb: PowBlock =>
        Try {
          // V: Is this the coinbase transaction? G: yes
          val proposition: PublicKey25519Proposition = pb.generatorProposition
          val nonce: Nonce = SimpleBoxTransactionBitcoin.nonceFromDigest(idToBytes(mod.id))
          val value: Value = Value @@ 83L
          val minerBox = PublicKey25519NoncedBox(proposition, nonce, value)

          val initial = (Seq(): Seq[Array[Byte]], Seq(): Seq[PublicKey25519NoncedBox], 0L)

          val (toRemove: Seq[ADKey@unchecked], toAdd: Seq[PublicKey25519NoncedBox], reward) =
            pb.transactions.foldLeft(initial) { case ((sr, sa, f), tx) =>
              ((sr ++ tx.boxIdsToOpen.toSet).map(id => ADKey @@ id), sa ++ tx.newBoxes.toSet, f + tx.fee)
            }

          @SuppressWarnings(Array("org.wartremover.warts.Product","org.wartremover.warts.Serializable"))
          val ops: Seq[BoxStateChangeOperation[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
            toRemove.map(id => Removal[PublicKey25519Proposition, PublicKey25519NoncedBox](id)) ++
              toAdd.map(b => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](b)) ++
              Seq(Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](minerBox))
          BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](ops)
        }
    }
  }

  def readOrGenerate(settings: ScorexSettings): BitcoinBoxStoredState = {
    import settings.dataDir
    dataDir.mkdirs()

    val iFile = new File(s"${dataDir.getAbsolutePath}/state")
    iFile.mkdirs()
    val stateStorage = new LSMStore(iFile, maxJournalEntryCount = 10000)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        stateStorage.close()
      }
    })
    val version = bytesToVersion(stateStorage.lastVersionID.map(_.data).getOrElse(Array.emptyByteArray))

    BitcoinBoxStoredState(stateStorage, version)
  }

  def genesisState(settings: ScorexSettings, initialBlocks: Seq[BitcoinBlock]): BitcoinBoxStoredState = {
    initialBlocks.foldLeft(readOrGenerate(settings)) { (state, mod) =>
      state.changes(mod).flatMap(cs => state.applyChanges(cs, idToVersion(mod.id))).get
    }
  }
}
