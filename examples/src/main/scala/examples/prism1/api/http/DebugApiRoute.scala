package examples.prism1.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import examples.commons.{SimpleBoxTransactionMemPool, SimpleBoxTransactionPrism}
import examples.prism1.blocks.{HybridBlock, PowBlock}
import examples.prism1.history.HybridHistory
import examples.prism1.state.HBoxStoredState
import examples.prism1.wallet.HBoxWallet
import io.circe.syntax._
import scorex.core.bytesToId
import scorex.core.api.http.{ApiResponse, ApiRouteWithFullView}
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.ScorexEncoding

import scala.util.Try


case class DebugApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory)
  extends ApiRouteWithFullView[HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionMemPool] 
    with ScorexEncoding {

  override val route: Route = (pathPrefix("debug") & withCors) {
    infoRoute ~ chain ~ delay ~ myblocks ~ generators ~ fullchain ~ briefchain ~ diffchain ~ timechain ~ allblocks ~ txCountchain ~ txchain
  }

  def delay: Route = {
    (get & path("delay" / Segment / IntNumber)) { case (encodedSignature, count) =>
      withNodeView { view =>
        val result: Try[String] = for {
          id <- encoder.decode(encodedSignature)
          delay <- view.history.averageDelay(bytesToId(id), count)
        } yield delay.toString
        ApiResponse("delay" -> result.getOrElse("Undefined"))
      }
    }
  }

  def infoRoute: Route = (get & path("info")) {
    withNodeView { view =>
      val bestBlockJson = view.history.bestBlock match {
        case _ => view.history.bestBlock.asInstanceOf[PowBlock].asJson
      }

      ApiResponse(
        "height" -> view.history.height.toString.asJson,
        "bestPoW" -> encoder.encodeId(view.history.bestPowId).asJson,
        "bestBlock" -> bestBlockJson,
        "stateVersion" -> encoder.encodeVersion(view.state.version).asJson
      )
    }
  }

  def myblocks: Route = (get & path("myblocks")) {
    withNodeView { view =>
      val pubkeys = view.vault.publicKeys

      def isMyPowBlock(b: HybridBlock): Boolean = b match {
        case pow: PowBlock => pubkeys.exists(pk => java.util.Arrays.equals(pk.pubKeyBytes, pow.generatorProposition.pubKeyBytes))
        case _ => false
      }

      val powCount = view.history.count(isMyPowBlock)

      ApiResponse(
        "pubkeys" -> pubkeys.map(pk => encoder.encode(pk.pubKeyBytes)).asJson,
        "count" -> (powCount).asJson,
        "powCount" -> powCount.asJson
      )
    }
  }

  def generators: Route = (get & path("generators")) {
    withNodeView { view =>
      val map: Map[String, Int] = view.history.generatorDistribution()
        .map(d => encoder.encode(d._1.pubKeyBytes) -> d._2)
      ApiResponse(map.asJson)
    }
  }

  def chain: Route = (get & path("chain")) {
    withNodeView { view =>
      ApiResponse("history" -> view.history.toString)
    }
  }

  def fullchain: Route = (get & path("fullchain")) {
    withNodeView { view =>
      val fc = view.history.lastPowBlocks(Int.MaxValue, view.history.bestPowBlock).map {
        b => b.toString
      }
      ApiResponse(fc)
    }
  }

  def briefchain: Route = (get & path("briefchain")) {
    withNodeView { view =>
      val fc = view.history.toString.split(",").map{ s => s.substring(0,6)}
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

  def timechain: Route = (get & path("timechain")) {
    withNodeView { view =>
      val lastBlocks = view.history.lastPowBlocks(Int.MaxValue, view.history.bestPowBlock)
      val timestamps = Array(1481110008516L)++ lastBlocks.map { b => b.timestamp}
      val fc = lastBlocks.zipWithIndex.map {
        case (b,i) => s"${encoder.encodeId(b.id).substring(0,6)} (${view.history.storage.getPoWDifficulty(Some(b.id)).toString};" +
          s"${timestamps(i+1)-timestamps(i)})"
      }
      ApiResponse("history" -> fc.mkString(" <- "))
    }
  }

  def allblocks: Route = (get & path("allblocks")) {
    withNodeView { view =>
      val fc = view.history.lastPowBlocks(Int.MaxValue, view.history.bestPowBlock).map {
        b => b.toString
      }
      val ab = view.history.storage.getAllBlocks.map {
        b => b.toString
      }//all blocks
      val rb = ab.filterNot(fc.toSet)//remain blocks that are not in the chain
      val rbOrphan = rb.map( s => "Orphan" + s )
      ApiResponse(fc ++ rbOrphan)
    }
  }

  def txCountchain: Route = (get & path("txcountchain")) {
    withNodeView { view =>
      val fc = view.history.lastPowBlocks(Int.MaxValue, view.history.bestPowBlock).map {
        b => s"${encoder.encodeId(b.id).substring(0,6)} (${b.txs.length})"
      }
      ApiResponse("history" -> fc.mkString(" <- "))
    }
  }

  def txchain: Route = (get & path("txchain")) {
    withNodeView { view =>
      val fc = view.history.lastPowBlocks(Int.MaxValue, view.history.bestPowBlock).foldLeft(Seq[SimpleBoxTransactionPrism]()) {
        (a,b) =>
          a ++ b.txs
      }
      ApiResponse(fc)
    }
  }
}
