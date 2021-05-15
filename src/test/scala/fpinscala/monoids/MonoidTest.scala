package fpinscala.monoids

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class MonoidTest extends AnyFunSuite {
  test("[10.1] test intAddition") {
    val monoid = Monoid.intAddition
    monoid.op(monoid.zero, 1) should be(
      monoid.op(1, monoid.zero)
    )
  }

  test("[10.1] test intMultiplication") {
    val monoid = Monoid.intMultiplication
    monoid.op(monoid.zero, 1) should be(
      monoid.op(1, monoid.zero)
    )

  }

  test("[10.1] test booleanOr") {
    val monoid = Monoid.booleanOr
    monoid.op(monoid.zero, true) should be(
      monoid.op(true, monoid.zero)
    )
    monoid.op(monoid.zero, false) should be(
      monoid.op(false, monoid.zero)
    )
  }

  test("[10.1] test booleanAnd") {
    val monoid = Monoid.booleanAnd
    monoid.op(monoid.zero, true) should be(
      monoid.op(true, monoid.zero)
    )
    monoid.op(monoid.zero, false) should be(
      monoid.op(false, monoid.zero)
    )
  }

  test("[10.2] test optionMonoid") {
    val monoid: Monoid[Option[Int]] = Monoid.optionMonoid
    monoid.op(monoid.zero, Some(1)) should be(
      monoid.op(Some(1), monoid.zero)
    )
    monoid.op(Some(1), monoid.zero) should be(
      monoid.op(monoid.zero, Some(1))
    )
  }

  test("[10.3] test endoMonoid") {
    val monoid: Monoid[Int => Int] = Monoid.endoMonoid
    val f: Int => Int = _ + 12
    monoid.op(monoid.zero, f).apply(11) should be(
      monoid.op(f, monoid.zero).apply(11)
    )
    monoid.op(f, monoid.zero).apply(11) should be(
      monoid.op(monoid.zero, f).apply(11)
    )

  }

  test("[10.X] set String Monoid") {
    val stringMonoid = Monoid.stringMonoid
    val words = List("Hic", "Est", "Index")
    val r = words.foldRight(stringMonoid.zero)(stringMonoid.op)
    val l = words.foldLeft(stringMonoid.zero)(stringMonoid.op)
    r should be(l)
  }

  test("[10.5] test foldMap") {
    val stringMonoid = Monoid.stringMonoid
    val ints = List(1, 2, 3, 4, 5)
    Monoid.foldMap(ints, stringMonoid)(_.toString) should be("12345")
  }

  test("[10.7] test foldMapV") {
    val stringMonoid = Monoid.stringMonoid
    val ints = List(1, 2, 3, 4, 5, 6, 7).toIndexedSeq
    Monoid.foldMapV(ints, stringMonoid)(_.toString) should be("1234567")
  }

}
