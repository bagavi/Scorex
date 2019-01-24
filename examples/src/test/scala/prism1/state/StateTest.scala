package prism1.state

import examples.prism1.state.HBoxStoredState
import org.scalatest.PropSpec
import prism1.StateGenerators

class StateTest extends PropSpec with StateGenerators {

  property("get a valid state") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val state: HBoxStoredState = stateGen.sample.get
  }

}
