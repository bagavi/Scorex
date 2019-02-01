package prism1.state

import examples.prism1.blocks.HybridBlock
import examples.prism1.state.HBoxStoredState
import prism1.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.state.{Insertion, Removal}
import scorex.testkit.properties.state.StateTests

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class HBoxStoredStateSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators
  with StateTests[HybridBlock, HBoxStoredState] {

  property("added boxes are always there") {
    forAll(stateGen){state =>
      var st = state
      check(checksToMake) { _ =>
        val c = stateChangesGenerator(state).sample.get
        st = st.applyChanges(c, versionTagGen.sample.get).get
        c.toAppend.foreach { case Insertion(b) =>
            st.closedBox(b.id) shouldBe Some(b)
        }
        c.toRemove.foreach { case Removal(bid) =>
          st.closedBox(bid) shouldBe None
        }
      }
    }
  }
}
