package examples.prism1.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import examples.commons.SimpleBoxTransactionMemPool
import examples.prism1.history.HybridHistory
import examples.prism1.state.HBoxStoredState
import examples.prism1.wallet.HBoxWallet
import io.circe.syntax._
import scorex.core.api.http.{ApiResponse, ApiRouteWithFullView, ApiTry}
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.ScorexEncoding
import scorex.util.ModifierId

import scala.util.Try

case class StatsApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory)
  extends ApiRouteWithFullView[HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionMemPool]
    with ScorexEncoding {

  override val route: Route = (pathPrefix("stats") & withCors) {
    tail ~ meanDifficulty ~ txCountchain ~ diffchain ~ timechain ~ minerId
  }

  def tail: Route = (get & path("tail" / IntNumber)) { num =>
    withNodeView { view =>
      val count = if (num>0) num else Int.MaxValue
      val lastBlockIds = view.history.lastBlockIds(view.history.bestBlock, count)
      val tail = lastBlockIds.map(id => encoder.encodeId(id).asJson)
      ApiResponse("count" -> count.asJson, "tail" -> tail.asJson)
    }
  }

  def meanDifficulty: Route = (get & path("meanDifficulty" / IntNumber / IntNumber)) { (start, end) =>
    withNodeView { view =>
      ApiTry {
        val count = (view.history.height - start).toInt
        val ids: Seq[ModifierId] = view.history.lastBlockIds(view.history.bestBlock, count).take(end - start)
        val powDiff = ids.flatMap(id => Try(view.history.storage.getPoWDifficulty(Some(id))).toOption)
        ApiResponse(
          "powDiff" -> (powDiff.sum / powDiff.length).asJson
        )
      }
    }
  }

  def timechain: Route = (get & path("timechain")) {
    withNodeView { view =>
      val lastBlocks = view.history.lastPowBlocks(Int.MaxValue, view.history.bestPowBlock)
      val timestamps = Array(1481110008516L)++ lastBlocks.map { b => b.timestamp}
      val fc = lastBlocks.zipWithIndex.map {
        case (b,i) => s"${encoder.encodeId(b.id).substring(0,6)} (${timestamps(i+1)-timestamps(i)})"
      }
      ApiResponse("history" -> fc.mkString(" <- "))
    }
  }

  def diffchain: Route = (get & path("diffchain")) {
    withNodeView { view =>
      val fc = view.history.lastPowBlocks(Int.MaxValue, view.history.bestPowBlock).map {
        b => s"${encoder.encodeId(b.id).substring(0,6)} (${view.history.storage.getPoWDifficulty(Some(b.id)).toString})"
      }
      ApiResponse("history" -> fc.mkString(" <- "))
    }
  }


  def txCountchain: Route = (get & path("txcountchain")) {
    withNodeView { view =>
      val fc = view.history.lastPowBlocks(Int.MaxValue, view.history.bestPowBlock).map {
        b => s"${encoder.encodeId(b.id).substring(0,6)} (${b.transactions.length})"
      }
      ApiResponse("history" -> fc.mkString(" <- "))
    }
  }


  def minerId: Route = (get & path("minerid")) {
    withNodeView { view =>
      val fc = view.history.lastPowBlocks(Int.MaxValue, view.history.bestPowBlock).map {
        b => s"${encoder.encodeId(b.id).substring(0,6)} (${b.minerId.substring(0,6)})"
      }
      ApiResponse("history" -> fc.mkString(" <- "))
    }
  }

}
