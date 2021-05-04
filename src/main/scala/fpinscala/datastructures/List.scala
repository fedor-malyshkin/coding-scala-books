package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`

case object Nil extends List[Nothing] // A `List` data constructor representing the empty list

/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double =
    ds match {
      case Nil          => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs)  => x * product(xs)
    }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((el, acc) => Cons(el, acc))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    foldLeft(reverse(as), z)((a, b) => f(b, a))

  def foldRight3[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def sum2(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil              => Nil
    case Cons(head, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil              => Nil
    case Cons(head, tail) => Cons(h, tail)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil                    => Nil
    case Cons(_, tail) if n > 0 => drop(tail, n - 1)
    case _                      => l
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil                         => Nil
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _                           => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil                    => Nil
    case Cons(h, t) if t == Nil => Nil
    case Cons(h, t)             => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = List.foldLeft(l, 0)((l, _) => l + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](l: List[A]): List[A] = List.foldLeft(l, Nil: List[A])((acc, el) => Cons(el, acc))

  def concat[A](xss: List[List[A]]): List[A] = foldLeft(xss, Nil: List[A])(append)

  def addOne(xs: List[Int]): List[Int] = foldRight(xs, Nil: List[Int])((e, a) => Cons(e + 1, a))

  def doubleToString(xs: List[Double]): List[String] =
    foldRight(xs, Nil: List[String])((e, a) => Cons(e.toString, a))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((e, a) => Cons(f(e), a))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil                => Nil
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(h, t)         => filter(t)(f)
  }

  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((e, acc) => if (f(e)) Cons(e, acc) else acc)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterByFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(e => if (f(e)) Cons(e, Nil) else Nil)

  def zipInt(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(e1, t1), Cons(e2, t2)) => Cons(e1 + e2, zipInt(t1, t2))
    }

  def zipWith[A, B](a: List[A], b: List[A])(f: (A, A) => B): List[B] =
    (a, b) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(e1, t1), Cons(e2, t2)) => Cons(f(e1, e2), zipWith(t1, t2)(f))
    }

}
