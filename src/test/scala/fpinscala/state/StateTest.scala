package fpinscala.state

import fpinscala.state.RNG.nonNegativeInt
import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class StateTest extends AnyFunSuite {

  test("testProductElementName") {}

  test("testProductElementNames") {}

  test("testFlatMap") {}

  test("testProductPrefix") {}

  test("testProductIterator") {}

  test("testMap") {}

  test("testMap2") {}

  test("testCopy") {}

  test("testRun") {}

  test("testUnapply") {}

  test("testApply") {}

  test("testSimulateMachine") {}
  test("[6.1] test nonNegativeInt") {
    val rng = RNG.Simple(11)
    nonNegativeInt(rng)._1 should be(4232237)
    nonNegativeInt(rng)._1 should be(4232237)
  }

  test("[6.2] test double") {
    val rng = RNG.Simple(11)
    RNG.double(rng)._1 shouldBe 0.001970788930 +- 0.000001
    RNG.double(rng)._1 shouldBe 0.001970788930 +- 0.000001
  }
}
