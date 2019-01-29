package prism1.state

import examples.prism1.state.HBoxStoredState
import org.scalatest.PropSpec
import prism1.HybridGenerators

class StateTest extends PropSpec with HybridGenerators {

  property("get a valid state") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val state: HBoxStoredState = stateGen.sample.get
//    state.store.getAll().map(kv => println(kv._1,kv._2))
  }

}
