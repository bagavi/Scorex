package examples.bitcoin

import akka.actor.ActorRef
import examples.commons.{SimpleBoxTransactionBitcoin, SimpleBoxTransactionBitcoinCompanion, SimpleBoxTransactionBitcoinMemPool}
import examples.bitcoin.api.http.{DebugApiRoute, StatsApiRoute, WalletApiRoute}
import examples.bitcoin.blocks._
import examples.bitcoin.history.{BitcoinHistory, BitcoinSyncInfo, BitcoinSyncInfoMessageSpec}
import examples.bitcoin.mining._
import examples.bitcoin.wallet.SimpleBoxTransactionGeneratorRef
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.core.api.http.{ApiRoute, NodeViewApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.message.MessageSpec
import scorex.core.network.{NodeViewSynchronizerRef, PeerFeature}
import scorex.core.serialization.{Serializer, SerializerRegistry}
import scorex.core.serialization.SerializerRegistry.SerializerRecord
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.Transaction

import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

class BitcoinApp(val settingsFilename: String) extends Application {

  import examples.bitcoin.wallet.SimpleBoxTransactionGenerator.ReceivableMessages.StartGeneration

  override type TX = SimpleBoxTransactionBitcoin
  override type PMOD = BitcoinBlock
  override type NVHT = BitcoinNodeViewHolder

  val bitcoinSettings: BitcoinSettings = BitcoinSettings.read(Some(settingsFilename))
  implicit override lazy val settings: ScorexSettings = BitcoinSettings.read(Some(settingsFilename)).scorexSettings

  log.debug(s"Starting application with settings \n$settings")

  implicit val serializerReg: SerializerRegistry = SerializerRegistry(Seq(SerializerRecord(SimpleBoxTransactionBitcoin.simpleBoxEncoder)))

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(BitcoinSyncInfoMessageSpec)

  override protected lazy val features: Seq[PeerFeature] = Seq()

  override val nodeViewHolderRef: ActorRef = BitcoinNodeViewHolderRef(bitcoinSettings, timeProvider)

  override val apiRoutes: Seq[ApiRoute] = Seq[ApiRoute](
    DebugApiRoute(settings.restApi, nodeViewHolderRef),
    WalletApiRoute(settings.restApi, nodeViewHolderRef),
    StatsApiRoute(settings.restApi, nodeViewHolderRef),
    UtilsApiRoute(settings.restApi),
    NodeViewApiRoute[SimpleBoxTransactionBitcoin](settings.restApi, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkControllerRef, timeProvider, settings.restApi)
  )

  override val swaggerConfig: String = Source.fromResource("api/testApi.yaml").getLines.mkString("\n")

  val miner: ActorRef = PowMinerRef(nodeViewHolderRef, bitcoinSettings.mining)

  val localInterface: ActorRef = BitcoinLocalInterfaceRef(nodeViewHolderRef, miner, bitcoinSettings.mining)

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(NodeViewSynchronizerRef.props[SimpleBoxTransactionBitcoin, BitcoinSyncInfo, BitcoinSyncInfoMessageSpec.type,
      BitcoinBlock, BitcoinHistory, SimpleBoxTransactionBitcoinMemPool]
      (networkControllerRef, nodeViewHolderRef,
        BitcoinSyncInfoMessageSpec, settings.network, timeProvider, BitcoinApp.modifierSerializers))

  if (settings.network.nodeName.startsWith("generatorNode")) {
    log.info("Starting transactions generation")
    val generator: ActorRef = SimpleBoxTransactionGeneratorRef(nodeViewHolderRef)
    generator ! StartGeneration(bitcoinSettings.mining.txGenerationRate)
  }
}

object BitcoinApp extends App {
  def modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
    Map(PowBlock.ModifierTypeId -> PowBlockCompanion,
      Transaction.ModifierTypeId -> SimpleBoxTransactionBitcoinCompanion)

  private val settingsFilename = args.headOption.getOrElse("settings.conf")
  new BitcoinApp(settingsFilename).run()

}
