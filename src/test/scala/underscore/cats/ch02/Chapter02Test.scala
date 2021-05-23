package underscore.cats.ch02

import scala.tools.nsc.doc.html.HtmlTags.A

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Chapter02Test extends AnyFunSuite {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }
  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) =
      monoid
  }

  test("[exercise 2.3]") {
    // AND
    val andBooleanMonoid = new Monoid[Boolean] {
      override def empty: Boolean = true
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

    andBooleanMonoid.combine(andBooleanMonoid.empty, true) should be(
      andBooleanMonoid.combine(true, andBooleanMonoid.empty)
    )
    andBooleanMonoid.combine(andBooleanMonoid.empty, false) should be(
      andBooleanMonoid.combine(false, andBooleanMonoid.empty)
    )

    val boolCombinations = List(true, false)
    for {
      v1 <- boolCombinations
      v2 <- boolCombinations
    } andBooleanMonoid.combine(v1, v2) should be(andBooleanMonoid.combine(v2, v1))

    // OR
    val orBooleanMonoid = new Monoid[Boolean] {
      override def empty: Boolean = false
      override def combine(x: Boolean, y: Boolean): Boolean = x || y
    }

    orBooleanMonoid.combine(orBooleanMonoid.empty, true) should be(
      orBooleanMonoid.combine(true, orBooleanMonoid.empty)
    )
    orBooleanMonoid.combine(orBooleanMonoid.empty, false) should be(
      orBooleanMonoid.combine(false, orBooleanMonoid.empty)
    )

    val boolCombinationsOR = List(true, false)
    for {
      v1 <- boolCombinationsOR
      v2 <- boolCombinationsOR
    } orBooleanMonoid.combine(v1, v2) should be(orBooleanMonoid.combine(v2, v1))

  }

  test("[exercise 2.4]") {
    def setMonoid[A]() = new Monoid[Set[A]] {
      override def empty: Set[A] = Set()
      override def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
    }

    val v = Set(1, 2, 3)
    val intSetMonoid = setMonoid[Int]()
    intSetMonoid.combine(intSetMonoid.empty, v) should be(
      intSetMonoid.combine(v, intSetMonoid.empty)
    )
  }
}
