package prism1.validation

import examples.prism1.validation.SemanticBlockValidator
import prism1.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Blake2b256


class SemanticBlockValidatorSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {

  private val validator = new SemanticBlockValidator(Blake2b256)

  property("Generated PoW block semantics is valid") {
    forAll(powBlockGen) { powBlock =>
      validator.validate(powBlock).get
    }
  }
}
