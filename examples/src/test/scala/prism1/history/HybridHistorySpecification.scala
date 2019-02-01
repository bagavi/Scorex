package prism1.history

import examples.prism1.history.{HybridHistory, HybridSyncInfo}
import prism1.HybridGenerators
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.ModifierTypeId
import scorex.core.consensus.History.{Equal, HistoryComparisonResult, Older, Younger}
import scorex.core.utils.ScorexEncoding
import scorex.util.ModifierId

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
class HybridHistorySpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators
  with ScorexEncoding {

  //make forAll try for minSuccessful = HybridHistory.DifficultyRecalcPeriod times
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfig(minSuccessful = HybridHistory.DifficultyRecalcPeriod)

  var history: HybridHistory = historyGen.sample.get

  //Generate chain
  property("Block application and HybridHistory.continuationIds") {
    var ids: Seq[ModifierId] = Seq()
    forAll(powBlockGen) { powR =>
      if (history.height <= HybridHistory.DifficultyRecalcPeriod) {
        val powBlock = powR.copy(parentId = history.bestPowId)
        history = history.append(powBlock).get._1
        history.bestBlock.encodedId shouldBe powBlock.encodedId

        history.modifierById(powBlock.id).isDefined shouldBe true
        ids = ids ++ Seq(powBlock.id)
      }
    }

    val startFrom = Seq((ModifierTypeId @@ 2.toByte, ids.head))//2 means Block

    history.continuationIds(startFrom, ids.length).get.map(_._2).map(encoder.encodeId) shouldEqual ids.map(encoder.encodeId)

    ids.length shouldBe HybridHistory.DifficultyRecalcPeriod

    //continuationIds with limit
    forAll(Gen.choose(0, ids.length - 1)) { startIndex: Int =>
      val startFrom = Seq((ModifierTypeId @@ 2.toByte, ids(startIndex)))
      val startList = ids.take(startIndex + 1).map(a => (ModifierTypeId @@ 2.toByte, a))
      val restIds = ids.zipWithIndex.filter { case (datum, index) => index >= startIndex }.map(_._1).map(encoder.encodeId)

      history.continuationIds(startFrom, ids.length).get.map(_._2).map(encoder.encodeId) shouldEqual restIds
      history.continuationIds(startList, ids.length).get.map(_._2).map(encoder.encodeId) shouldEqual restIds

      val limit = 5
      val continuation = history.continuationIds(startList, limit).get
      continuation.length shouldBe Math.min(limit, restIds.length)
      startList.exists(sl => sl._2 == continuation.head._2) shouldBe true
      continuation.tail.foreach { c =>
        startList.exists(sl => sl._2 == c._2) shouldBe false
      }
    }
  }

  property("History comparison") {
    (history.height >= HybridHistory.DifficultyRecalcPeriod) shouldBe true
    //TODO test for completed pairs

    testHistory(history)
  }

  def compareAndCheck(history: HybridHistory, syncInfo: HybridSyncInfo, continuationSize: Int = 10): HistoryComparisonResult = {
    val extensionOpt = history.continuationIds(syncInfo.startingPoints, continuationSize)
    val comparison = history.compare(syncInfo)
    if (comparison == Younger) {
      println(extensionOpt)
      extensionOpt.nonEmpty shouldBe true
    }
    comparison
  }

  def testHistory(history: HybridHistory): Unit = {
    val equalsSyncInfo: HybridSyncInfo = history.syncInfo
    val lastIds = equalsSyncInfo.lastPowBlockIds
    lastIds.last shouldEqual history.bestPowId
    compareAndCheck(history, equalsSyncInfo) shouldBe Equal
    compareAndCheck(history, equalsSyncInfo.copy(lastPowBlockIds = lastIds.tail)) shouldBe Equal

    val youngerSyncInfo = equalsSyncInfo.copy(lastPowBlockIds = lastIds.dropRight(1))
    compareAndCheck(history, youngerSyncInfo) shouldBe Younger

    val betterForkSyncInfo = equalsSyncInfo
      .copy(lastPowBlockIds = lastIds.dropRight(1).tail ++ Array(modifierIdGen.sample.get, modifierIdGen.sample.get))

    compareAndCheck(history, betterForkSyncInfo) shouldBe Older
  }
}
