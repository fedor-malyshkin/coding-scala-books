package fpinscala.gettingstarted

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class GettingStartedTest extends AnyFunSuite {

  test("[2.1] ") {
    MyModule.fib(2) should be(1)
    MyModule.fib(3) should be(1)
    MyModule.fib(4) should be(2)
    MyModule.fib(5) should be(3)
    MyModule.fib(6) should be(5)
  }

  test("[2.2]") {
    def isGreater(first: Int, second: Int): Boolean = first >= second

    PolymorphicFunctions.isSorted(Array(6, 5, 4, 3, 2, 1), isGreater) should be(true)
    PolymorphicFunctions.isSorted(Array(6, 5, 4, 2, 3, 1), isGreater) should be(false)
  }
}
