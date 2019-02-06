package examples.prism1.history


import java.io.File

import examples.commons.{FileLogger, SimpleBoxTransactionPrism}
import examples.prism1.blocks.{HybridBlock, PowBlock}
import examples.prism1.mining.HybridMiningSettings
import examples.prism1.validation.{DifficultyBlockValidator, ParentBlockValidator, SemanticBlockValidator}
import io.iohk.iodb.LSMStore
import scorex.core.block.{Block, BlockValidator}
import scorex.core.consensus.History._
import scorex.core.consensus.ModifierSemanticValidity._
import scorex.core.consensus._
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.{NetworkTimeProvider, ScorexEncoding}
import scorex.core.validation.RecoverableModifierError
import scorex.core.{ModifierTypeId, NodeViewModifier, bytesToId}
import scorex.crypto.hash.Blake2b256
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * History storage
  * we store all the blocks, even if they are not in a main chain
  */
class HybridHistory(val storage: HistoryStorage,
                    settings: HybridMiningSettings,
                    validators: Seq[BlockValidator[HybridBlock]],
                    statsLogger: Option[FileLogger],
                    timeProvider: NetworkTimeProvider)
  extends History[HybridBlock, HybridSyncInfo, HybridHistory] with ScorexLogging with ScorexEncoding {

  import HybridHistory._

  override type NVCT = HybridHistory

  require(NodeViewModifier.ModifierIdSize == 32, "32 bytes ids assumed")

  val height: Long = storage.height


  val bestPowId: ModifierId = storage.bestPowId
  lazy val bestPowBlock: PowBlock = storage.bestPowBlock
  lazy val bestBlock: HybridBlock = bestPowBlock

  /**
    * Return specified number of PoW blocks, ordered back from last one
    *
    * @param count - how many blocks to return
    * @return PoW blocks, in reverse order (starting from the most recent one)
    */
  def lastPowBlocks(count: Int, startBlock: PowBlock): Seq[PowBlock] = if (isEmpty) {
    Seq()
  } else {
    @tailrec
    def loop(b: PowBlock, acc: Seq[PowBlock] = Seq()): Seq[PowBlock] = if (acc.length >= count) {
      acc
    } else {
      modifierById(b.parentId) match {
        case Some(parent: PowBlock) => loop(parent, b +: acc)
        case _ => b +: acc
      }
    }

    loop(startBlock)
  }

  // TODO: review me .get
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def lastBlockIds(startBlock: HybridBlock, count: Int): Seq[ModifierId] = {
    chainBack(startBlock, isGenesis, count - 1).get.map(_._2)
  }

  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = height <= 0

  override def modifierById(id: ModifierId): Option[HybridBlock with
    Block[SimpleBoxTransactionPrism]] = storage.modifierById(id)

  override def contains(id: ModifierId): Boolean = id == settings.GenesisParentId || modifierById(id).isDefined

  private def powBlockAppend(powBlock: PowBlock): (HybridHistory, ProgressInfo[HybridBlock]) = {
    val progress: ProgressInfo[HybridBlock] = if (isGenesis(powBlock)) {
      storage.update(powBlock, Some(settings.initialDifficulty), isBest = true)
      ProgressInfo(None, Seq(), Seq(powBlock), Seq())
    } else {
      storage.heightOf(powBlock.parentId) match {
        case Some(_) =>

          //potentially the best block, if its not a block in a fork containing invalid block
          val isBest: Boolean = storage.height == storage.parentHeight(powBlock)

          val mod: ProgressInfo[HybridBlock] = if (isBest) {
            if (isGenesis(powBlock) || powBlock.parentId == bestPowId ) {
//              log.info(s"New best PoW block ${encoder.encodeId(powBlock.id)} at height ${storage.height}")
              //just apply one block to the end
              ProgressInfo(None, Seq(), Seq(powBlock), Seq())
            } else {
              // ToDo: Vivek: I am not sure of the logic here
              //we're switching to a better chain, if it does not contain an invalid block
              bestForkChanges(powBlock)
            }
          } else {
            log.info(s"New orphaned PoW block ${encoder.encodeId(powBlock.id)}")
            ProgressInfo(None, Seq(), Seq(), Seq())
          }
          // Vivek: Activiate below once "getPoWDifficulty" is correct.
          val difficulties =  calcDifficultiesForNewBlock(powBlock)
          storage.update(powBlock, Some(difficulties), isBest)
          mod

        case None =>
          // Gerui: try to throw an error since it should be an error!
          throw new RecoverableModifierError("Parent block not in history")
          //log.warn(s"No parent block ${powBlock.parentId} in history")
          //ProgressInfo[HybridBlock](None, Seq[HybridBlock](), Seq(), Seq())
      }
    }
    // require(modifications.toApply.exists(_.id sameElements powBlock.id))
    (new HybridHistory(storage, settings, validators, statsLogger, timeProvider), progress)
  }


  /**
    * @param block - block to append
    * @return
    */
  override def append(block: HybridBlock): Try[(HybridHistory, ProgressInfo[HybridBlock])] = Try {
    log.debug(s"Trying to append block ${encoder.encodeId(block.id)} to history")

    validators.map(_.validate(block)).foreach {
      case Failure(e) =>
        log.warn(s"Block validation failed", e)
        throw e
      case _ =>
    }

    val res: (HybridHistory, ProgressInfo[HybridBlock]) = block match {
      case powBlock: PowBlock => powBlockAppend(powBlock)
    }

    log.info(s"History: block ${encoder.encodeId(block.id)} appended to chain with score ${storage.heightOf(block.id)}. " +
      s"Best score is ${storage.bestChainScore}. " )
    statsLogger.foreach(l => l.appendString(timeProvider.time() + ":" +
      lastBlockIds(bestBlock, 50).map(encoder.encodeId).mkString(",")))
    res
  }

  /*
    Calculates the difficulty of the new block using the observed mining rate
    of previous "DifficultyRecalcPeriod" blocks.
   */
  private def calcDifficultiesForNewBlock(powBlock: PowBlock): BigInt = {
    def bounded(newVal: BigInt, oldVal: BigInt): BigInt = if (newVal > oldVal * 2) oldVal * 2 else newVal

    val powHeight = storage.parentHeight(powBlock)  + 1
    if (powHeight > DifficultyRecalcPeriod && powHeight % DifficultyRecalcPeriod == 0) {
//    if (powHeight > DifficultyRecalcPeriod) {

      //recalc difficulties

      // TODO: review me .get and asInstanceOf
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val lastPow = powBlock
      val powBlocks = lastPowBlocks(DifficultyRecalcPeriod, lastPow) //.ensuring(_.length == DifficultyRecalcPeriod)

      // TODO: fixme, What should we do if `powBlocksHead` is empty?
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
      val powBlocksHead = powBlocks.head
      val realTime = lastPow.timestamp - powBlocksHead.timestamp
      val expectedTime = DifficultyRecalcPeriod * settings.targetBlockDelay.toMillis
      val oldPowDifficulty = storage.getPoWDifficulty(Some(lastPow.parentId)) // Vivek: To change the storage.getPoWDifficulty

      val newPowDiffUnlimited = (oldPowDifficulty * expectedTime / realTime).max(BigInt(1L))
      val newPowDiff = bounded(newPowDiffUnlimited, oldPowDifficulty)

      log.info(s"PoW difficulty changed at ${encoder.encodeId(powBlock.id)}: old $oldPowDifficulty, new $newPowDiff. " +
//        s" last: $lastPow, head: $powBlocksHead, " +
        s" realTime ${realTime},  expectedTime  ${expectedTime}, ")
      newPowDiff
    } else {
      //Same difficulty as in previous block
      assert(modifierById(powBlock.parentId).isDefined, "Parent should always be in history")
      // TODO: review me .get and asInstanceOf
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val parentPoWId: ModifierId = modifierById(powBlock.parentId).get.asInstanceOf[PowBlock].id
      storage.getPoWDifficulty(Some(parentPoWId))
    }
  }


  // TODO: review me .get
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def bestForkChanges(block: HybridBlock): ProgressInfo[HybridBlock] = {
    val parentId = storage.parentId(block)
    val (newSuffix, oldSuffix) = commonBlockThenSuffixes(modifierById(parentId).get)
    log.info(s"Processing fork for block ${encoder.encodeId(block.id)}: \n" +
      s"old: ${oldSuffix.map(encoder.encodeId)}\n" +
      s"new: ${newSuffix.map(encoder.encodeId)}")

    val rollbackPoint = newSuffix.headOption

    val newSuffixValid = !newSuffix.drop(1).map(storage.semanticValidity).contains(Invalid)

    if (newSuffixValid) {
      // TODO: fixme, What should we do if `oldSuffix` is empty? and .get
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
      val throwBlocks = oldSuffix.tail.map(id => modifierById(id).get)
      // TODO: fixme, What should we do if `newSuffix` is empty? and .get
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
      val applyBlocks = newSuffix.tail.map(id => modifierById(id).get) ++ Seq(block)
      require(applyBlocks.nonEmpty)
      require(throwBlocks.nonEmpty)

      ProgressInfo[HybridBlock](rollbackPoint, throwBlocks, applyBlocks, Seq())
    } else {
      log.info(s"Orphaned block $block from invalid suffix")
      ProgressInfo(None, Seq(), Seq(), Seq())
    }
  }

  override def openSurfaceIds(): Seq[ModifierId] =
    if (isEmpty) Seq(settings.GenesisParentId)
    else Seq(bestPowId)

  override def applicableTry(block: HybridBlock): Try[Unit] = {
    block match {
      case pwb: PowBlock if !contains(pwb.parentId) =>
        Failure(new RecoverableModifierError("Parent block not in history yet"))
      case _ =>
        Success()
    }
  }

  def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[ModifierIds] = {
    def inList(m: HybridBlock): Boolean = idInList(m.id) || isGenesis(m)

    def idInList(id: ModifierId): Boolean = from.exists(f => f._2 == id)

    //Look without limit for case difference between nodes is bigger then size
    chainBack(bestBlock, inList) match {
      case Some(chain) if chain.exists(id => idInList(id._2)) => Some(chain.take(size))
      case Some(_) =>
        log.warn("Found chain without ids from remote")
        None
      case _ => None
    }
  }

  override def continuationIds(info: HybridSyncInfo,
                               size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = {
    continuationIds(info.startingPoints, size)
  }

  override def syncInfo: HybridSyncInfo =
    HybridSyncInfo(
      answer = false,
      lastPowBlocks(HybridSyncInfo.MaxLastPowBlocks, bestPowBlock).map(_.id))

  @tailrec
  private def divergentSuffix(otherLastPowBlocks: Seq[ModifierId],
                              suffixFound: Seq[ModifierId] = Seq()): Seq[ModifierId] = {
    // TODO: fixme, What should we do if `otherLastPowBlocks` is empty? Could we return Seq[ModifierId]() in that case?
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val head = otherLastPowBlocks.head
    val newSuffix = suffixFound :+ head
    modifierById(head) match {
      case Some(_) =>
        newSuffix
      case None => if (otherLastPowBlocks.length <= 1) {
        Seq()
      } else {
        // `otherLastPowBlocks.tail` is safe as its length is greater than 1
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val otherLastPowBlocksTail = otherLastPowBlocks.tail
        divergentSuffix(otherLastPowBlocksTail, newSuffix)
      }
    }
  }

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(other: HybridSyncInfo): HistoryComparisonResult = {
    val dSuffix = divergentSuffix(other.lastPowBlockIds.reverse)

    dSuffix.length match {
      case 0 =>
        log.warn(s"CompareNonsense: ${other.lastPowBlockIds.toList.map(encoder.encodeId)} at height $height}")
        Nonsense
      case 1 =>
        // `dSuffix.head` is safe as `dSuffix.length` is 1
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val dSuffixHead = dSuffix.head
        if (dSuffixHead == bestPowId) Equal //Vivek: Changed here with low confidence
        else Younger
      case _ =>
        // +1 to include common block
        // TODO: What would be a default value for `localSuffixLength` in order to remove the unsafe calls to get and tail?
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
        val localSuffixLength = storage.heightOf(bestPowId).get - storage.heightOf(dSuffix.last).get
        val otherSuffixLength = dSuffix.length

        if (localSuffixLength < otherSuffixLength) Older
        else if (localSuffixLength == otherSuffixLength) Equal
        else Younger
    }
  }

  lazy val powDifficulty: BigInt = storage.getPoWDifficulty(None)

  private def isGenesis(b: HybridBlock): Boolean = storage.isGenesis(b)

  def blockGenerator(m: HybridBlock): PublicKey25519Proposition = m match {
    case p: PowBlock => p.generatorProposition
  }

  def generatorDistribution(): Map[PublicKey25519Proposition, Int] = {
    val map = collection.mutable.Map[PublicKey25519Proposition, Int]().withDefaultValue(0)

    @tailrec
    def loop(m: HybridBlock): Unit = {
      val generator = blockGenerator(m)
      map.update(generator, map(generator) + 1)
      parentBlock(m) match {
        case Some(parent) => loop(parent)
        case None =>
      }
    }

    loop(bestBlock)
    map.toMap
  }

  def count(f: HybridBlock => Boolean): Int = filter(f).length

  def filter(f: HybridBlock => Boolean): Seq[ModifierId] = {
    @tailrec
    def loop(m: HybridBlock, acc: Seq[ModifierId]): Seq[ModifierId] = parentBlock(m) match {
      case Some(parent) => if (f(m)) loop(parent, m.id +: acc) else loop(parent, acc)
      case None => if (f(m)) m.id +: acc else acc
    }

    loop(bestBlock, Seq())
  }

  def parentBlock(m: HybridBlock): Option[HybridBlock] = m match {
    case b: PowBlock => modifierById(b.parentId)
  }

  /**
    * Go back though chain and get block ids until condition until
    * None if parent block is not in chain
    */
  @tailrec
  private def chainBack(m: HybridBlock,
                        until: HybridBlock => Boolean,
                        limit: Int = Int.MaxValue,
                        acc: Seq[(ModifierTypeId, ModifierId)] = Seq()): Option[Seq[(ModifierTypeId, ModifierId)]] = {
    @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
    val sum: Seq[(ModifierTypeId, ModifierId)] = (PowBlock.ModifierTypeId -> m.id) +: acc

    if (limit <= 0 || until(m)) {
      Some(sum)
    } else {
      parentBlock(m) match {
        case Some(parent) => chainBack(parent, until, limit - 1, sum)
        case _ =>
          log.warn(s"Parent block for ${encoder.encodeId(m.id)} not found ")
          None
      }
    }
  }

  /**
    * find common suffixes for two chains - starting from forkBlock and from bestPowBlock
    * returns last common block and then variant blocks for two chains,
    */
  final def commonBlockThenSuffixes(forkBlock: HybridBlock,
                                    limit: Int = Int.MaxValue): (Seq[ModifierId], Seq[ModifierId]) = {
    // TODO: Review me .get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val loserChain = chainBack(bestBlock, isGenesis, limit).get.map(_._2)

    def in(m: HybridBlock): Boolean = loserChain.exists(s => s == m.id)

    // TODO: Review me .get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val winnerChain = chainBack(forkBlock, in, limit).get.map(_._2)
    val i = loserChain.indexWhere { id =>
      winnerChain.headOption match {
        case None => false
        case Some(winnerChainHead) => id == winnerChainHead
      }
    }
    (winnerChain, loserChain.takeRight(loserChain.length - i))
  } ensuring { r =>
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val r1 = r._1.head
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val r2 = r._2.head
    r1 == r2
  }

  /**
    * Average delay in milliseconds between last $blockNum blocks starting from $block
    * Debug only
    */
  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
  def averageDelay(id: ModifierId, blockNum: Int): Try[Long] = Try {
    val block = modifierById(id).get
    val c = chainBack(block, isGenesis, blockNum).get.map(_._2)
    (block.timestamp - modifierById(c.head).get.timestamp) / c.length
  }

  //chain without brothers
  // TODO: review me .get
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  override def toString: String = {
    chainBack(storage.bestPowBlock, isGenesis).get.map(_._2).map(encoder.encodeId).mkString(",")
  }

  override def reportModifierIsValid(modifier: HybridBlock): HybridHistory = {
    storage.updateValidity(modifier, Valid)
    storage.update(modifier, None, isBest = true)

    new HybridHistory(storage, settings, validators, statsLogger, timeProvider)
  }

  override def reportModifierIsInvalid(modifier: HybridBlock,
                                       progressInfo: ProgressInfo[HybridBlock]): (HybridHistory,
    ProgressInfo[HybridBlock]) = {
    storage.updateValidity(modifier, Invalid)

    new HybridHistory(storage, settings, validators, statsLogger, timeProvider) ->
      ProgressInfo(None, Seq(), Seq(), Seq())
  }

  override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity =
    storage.semanticValidity(modifierId)
}


object HybridHistory extends ScorexLogging {
  val DifficultyRecalcPeriod: Int = 20

  def readOrGenerate(settings: ScorexSettings, minerSettings: HybridMiningSettings, timeProvider: NetworkTimeProvider): HybridHistory = {
    readOrGenerate(settings.dataDir, settings.logDir, minerSettings, timeProvider)
  }

  def readOrGenerate(dataDir: File, logDir: File, settings: HybridMiningSettings, timeProvider: NetworkTimeProvider): HybridHistory = {
    val iFile = new File(s"${dataDir.getAbsolutePath}/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile, maxJournalEntryCount = 10000)

    val logger = new FileLogger(logDir.getAbsolutePath + "/tails.data")

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing block storage...")
        blockStorage.close()
      }
    })

    val storage = new HistoryStorage(blockStorage, settings)
    val validators = Seq(new DifficultyBlockValidator(settings, storage),
      new ParentBlockValidator(storage),
      new SemanticBlockValidator(Blake2b256)
    )

    new HybridHistory(storage, settings, validators, Some(logger), timeProvider)
  }

}
