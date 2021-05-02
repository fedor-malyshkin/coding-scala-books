package fpinscala.gettingstarted

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class PolymorphicFunctionsTest extends AnyFunSuite {

  test("testCurry") {
    val r = PolymorphicFunctions.curry((a: Int, b: Int) => a + b)
    r(2)(3) should be(5)
  }

  test("testCompose") {
    val f1 = (i: Int) => i + 100
    val f2 = (i: Int) => i.toHexString
    val composed = PolymorphicFunctions.compose(f1, f2)
    composed(10) should be("6e")

  }

  test("testUncurry") {
    def inn(a: Int)(b: Int): Int = a - b

    val uncurr = PolymorphicFunctions.uncurry(inn)
    uncurr(10, 2) should be(8)

  }

}
