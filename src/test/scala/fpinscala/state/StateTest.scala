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

  test("[6.3] test intDouble") {
    val rng = RNG.Simple(11)
    RNG.intDouble(rng)._1 shouldBe (4232237, 0.08326200302690268)
    RNG.intDouble(rng)._1 shouldBe (4232237, 0.08326200302690268)
  }

  test("[6.3] test doubleInt") {
    val rng = RNG.Simple(11)
    RNG.doubleInt(rng)._1 shouldBe (0.08326200302690268, 4232237)
    RNG.doubleInt(rng)._1 shouldBe (0.08326200302690268, 4232237)
  }

  test("[6.3] test double3") {
    val rng = RNG.Simple(11)
    RNG.double3(rng)._1 shouldBe (0.0019707889296114445, 0.08326200302690268, 0.35328528471291065)
    RNG.double3(rng)._1 shouldBe (0.0019707889296114445, 0.08326200302690268, 0.35328528471291065)
  }

  test("[6.X] unit") {
    val t = RNG.unit(12)
    val rng = RNG.Simple(11)
    t.apply(rng) shouldBe (12, rng)
  }

  test("[6.5] test doubleByMap") {
    val rng = RNG.Simple(11)
    RNG.doubleByMap(rng)._1 shouldBe 0.001970788930 +- 0.000001
    RNG.doubleByMap(rng)._1 shouldBe 0.001970788930 +- 0.000001
  }

}
