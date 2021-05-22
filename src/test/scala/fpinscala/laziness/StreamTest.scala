package fpinscala.laziness

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class StreamTest extends AnyFunSuite {

  test("[5.1] test toList") {
    val str = Stream.cons(1, Stream.cons(2, Stream.cons(3, Empty)))
    str.toList should be(List(1, 2, 3))
  }

  test("[5.2] test take") {
    val str = Stream.cons(1, Stream.cons(2, Stream.cons(3, Empty)))
    str.toList should be(List(1, 2, 3))
  }

}
