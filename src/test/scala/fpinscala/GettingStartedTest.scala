package fpinscala
import fpinscala.gettingstarted.{MyModule, PolymorphicFunctions}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class GettingStartedTest extends AnyFlatSpec with Matchers {

  behavior of "GettingStartedTest"

  it should "EXERCISE 2.1" in {
    MyModule.fib(2) should be(1)
    MyModule.fib(3) should be(1)
    MyModule.fib(4) should be(2)
    MyModule.fib(5) should be(3)
    MyModule.fib(6) should be(5)
  }

  it should "EXERCISE 2.2" in {
    def isGreater(first: Int, second: Int): Boolean = first >= second

    PolymorphicFunctions.isSorted(Array(6, 5, 4, 3, 2, 1), isGreater) should be(true)

    PolymorphicFunctions.isSorted(Array(6, 5, 4, 2, 3, 1), isGreater) should be(false)
  }
}
