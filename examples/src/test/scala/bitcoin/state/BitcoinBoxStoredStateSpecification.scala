package bitcoin.state

import examples.bitcoin.blocks.BitcoinBlock
import examples.bitcoin.state.BitcoinBoxStoredState
import bitcoin.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.state.{Insertion, Removal}
import scorex.testkit.properties.state.StateTests

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class BitcoinBoxStoredStateSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators
  with StateTests[BitcoinBlock, BitcoinBoxStoredState] {

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
