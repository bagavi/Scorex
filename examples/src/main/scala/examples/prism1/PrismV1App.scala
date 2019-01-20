package examples.prism1

import akka.actor.ActorRef
import examples.commons.{SimpleBoxTransactionPrism, SimpleBoxTransactionPrismCompanion, SimpleBoxTransactionPrismMemPool}
import examples.prism1.api.http.{DebugApiRoute, StatsApiRoute, WalletApiRoute}
import examples.prism1.blocks._
import examples.prism1.history.{HybridHistory, HybridSyncInfo, HybridSyncInfoMessageSpec}
import examples.prism1.mining._
import examples.prism1.wallet.SimpleBoxTransactionPrismGeneratorRef
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

class PrismV1App(val settingsFilename: String) extends Application {

  import examples.prism1.wallet.SimpleBoxTransactionPrismGenerator.ReceivableMessages.StartGeneration

  override type TX = SimpleBoxTransactionPrism
  override type PMOD = HybridBlock
  override type NVHT = HybridNodeViewHolder

  private val hybridSettings = HybridSettings.read(Some(settingsFilename))
  implicit override lazy val settings: ScorexSettings = HybridSettings.read(Some(settingsFilename)).scorexSettings

  log.debug(s"Starting application with settings \n$settings")

  implicit val serializerReg: SerializerRegistry = SerializerRegistry(Seq(SerializerRecord(SimpleBoxTransactionPrism.simpleBoxEncoder)))

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(HybridSyncInfoMessageSpec)

  override protected lazy val features: Seq[PeerFeature] = Seq()

  override val nodeViewHolderRef: ActorRef = HybridNodeViewHolderRef(hybridSettings, timeProvider)

  override val apiRoutes: Seq[ApiRoute] = Seq[ApiRoute](
    DebugApiRoute(settings.restApi, nodeViewHolderRef),
    WalletApiRoute(settings.restApi, nodeViewHolderRef),
    StatsApiRoute(settings.restApi, nodeViewHolderRef),
    UtilsApiRoute(settings.restApi),
    NodeViewApiRoute[SimpleBoxTransactionPrism](settings.restApi, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkControllerRef, timeProvider, settings.restApi)
  )

  override val swaggerConfig: String = Source.fromResource("api/testApi.yaml").getLines.mkString("\n")

  val miner: ActorRef = PowMinerRef(nodeViewHolderRef, hybridSettings.mining)

  val localInterface: ActorRef = HLocalInterfaceRef(nodeViewHolderRef, miner, hybridSettings.mining)

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(NodeViewSynchronizerRef.props[SimpleBoxTransactionPrism, HybridSyncInfo, HybridSyncInfoMessageSpec.type,
      HybridBlock, HybridHistory, SimpleBoxTransactionPrismMemPool]
      (networkControllerRef, nodeViewHolderRef,
        HybridSyncInfoMessageSpec, settings.network, timeProvider, PrismV1App.modifierSerializers))

  if (settings.network.nodeName.startsWith("generatorNode")) {
    log.info("Starting transactions generation")
    val generator: ActorRef = SimpleBoxTransactionPrismGeneratorRef(nodeViewHolderRef)
    generator ! StartGeneration(1 seconds)
  }
}

object PrismV1App extends App {
  def modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
    Map(PowBlock.ModifierTypeId -> PowBlockCompanion,
      Transaction.ModifierTypeId -> SimpleBoxTransactionPrismCompanion)

  private val settingsFilename = args.headOption.getOrElse("settings.conf")
  new PrismV1App(settingsFilename).run()

}
