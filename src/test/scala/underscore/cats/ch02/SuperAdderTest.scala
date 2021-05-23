package underscore.cats.ch02

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class SuperAdderTest extends AnyFunSuite {

  test("[2.5.4] testAdd") {
    SuperAdder.add(List(1, 2, 3, 4, 5, 6, 7, 8, 9)) should be(45)
  }

  test("[2.5.4] testAddOptional") {
    SuperAdder.add(
      List(
        Option(1),
        Option(2),
        Option(3),
        Option(4),
        Option(5),
        Some(6),
        Option(7),
        Option(8),
        None
      )
    ) should be(Some(36))
  }

  test("[2.5.4] test add Order") {
    val o1 = Order(1, 2)
    val o2 = Order(3, 4)
    SuperAdder.add(List(o1, o2)) should be(Order(4.0, 6.0))
  }
}
