package bitcoin.state

import examples.bitcoin.state.BitcoinBoxStoredState
import org.scalatest.PropSpec
import bitcoin.BitcoinGenerators

class StateTest extends PropSpec with BitcoinGenerators {

  property("get a valid state") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val state: BitcoinBoxStoredState = stateGen.sample.get
//    state.store.getAll().map(kv => println(kv._1,kv._2))
  }

}
