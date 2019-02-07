package examples.bitcoin.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import examples.commons.{SimpleBoxTransactionBitcoin, SimpleBoxTransactionBitcoinMemPool, Value}
import examples.bitcoin.history.BitcoinHistory
import examples.bitcoin.state.BitcoinBoxStoredState
import examples.bitcoin.wallet.BitcoinBoxWallet
import io.circe.parser._
import io.circe.syntax._
import scorex.core.api.http.{ApiError, ApiResponse, ApiRouteWithFullView}
import scorex.core.settings.RESTApiSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexEncoding
import scorex.crypto.signatures.PublicKey

import scala.util.{Failure, Success, Try}


case class WalletApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                         (implicit val context: ActorRefFactory)
  extends ApiRouteWithFullView[BitcoinHistory, BitcoinBoxStoredState, BitcoinBoxWallet, SimpleBoxTransactionBitcoinMemPool]
    with ScorexEncoding {

  import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction

  //TODO move to settings?
  val DefaultFee: Int = 100

  override val route: Route = (pathPrefix("wallet") & withCors) {
    balances ~ transfer ~ multitransfer
  }

  def transfer: Route = (post & path("transfer")) {
    entity(as[String]) { body =>
      withAuth {
        withNodeView { view =>
          parse(body) match {
            case Left(failure) => ApiError(failure.getCause)
            case Right(json) => Try {
              val wallet = view.vault
              // TODO: Can we do this extraction in a safer way (not calling head/get)?
              @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
              val amount: Long = (json \\ "amount").head.asNumber.get.toLong.get
              // TODO: Can we do this extraction in a safer way (not calling head/get)?
              @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
              val recipient: PublicKey25519Proposition = PublicKey25519Proposition(PublicKey @@ encoder.decode((json \\ "recipient").head.asString.get).get)
              val fee: Long = (json \\ "fee").headOption.flatMap(_.asNumber).flatMap(_.toLong).getOrElse(DefaultFee)
              val tx = SimpleBoxTransactionBitcoin.create(wallet, Seq((recipient, Value @@ amount)), fee).get
              nodeViewHolderRef ! LocallyGeneratedTransaction[SimpleBoxTransactionBitcoin](tx)
              tx.asJson
            } match {
              case Success(resp) => ApiResponse(resp)
              case Failure(e) => ApiError(e)
            }
          }
        }
      }
    }
  }

  def multitransfer: Route = (post & path("multitransfer")) {
    entity(as[String]) { body =>
      withAuth {
        withNodeView { view =>
          parse(body) match {
            case Left(failure) => ApiError(failure.getCause)
            case Right(json) => Try {
              val wallet = view.vault
              // TODO: Can we do this extraction in a safer way (not calling head/get)?
              @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
              val amounts: Seq[Value] = (json \\ "amount").head.asArray.get.map(amount => Value @@ amount.asNumber.get.toLong.get)
              // TODO: Can we do this extraction in a safer way (not calling head/get)?
              @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
              val recipients: Seq[PublicKey25519Proposition] = (json \\ "recipient").head.asArray.get.map {
                pubkey => PublicKey25519Proposition(PublicKey @@ encoder.decode(pubkey.asString.get).get)
              }
              val fee: Long = (json \\ "fee").headOption.flatMap(_.asNumber).flatMap(_.toLong).getOrElse(DefaultFee)
              val tx = SimpleBoxTransactionBitcoin.create(wallet, recipients zip amounts, fee).get
              nodeViewHolderRef ! LocallyGeneratedTransaction[SimpleBoxTransactionBitcoin](tx)
              tx.asJson
            } match {
              case Success(resp) => ApiResponse(resp)
              case Failure(e) => ApiError(e)
            }
          }
        }
      }
    }
  }

  def balances: Route = (get & path("balances")) {
    withNodeView { view =>
      val wallet = view.vault
      val boxes = wallet.boxes().map(_.box)
      ApiResponse(
        "totalBalance" -> boxes.map(_.value.toLong).sum.toString.asJson,
        "publicKeys" -> wallet.publicKeys.map(pk => encoder.encode(pk.pubKeyBytes)).asJson,
        "boxes" -> boxes.asJson
      )
    }
  }

}
