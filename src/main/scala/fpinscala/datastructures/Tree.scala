package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
object NilLeaf extends Tree[Nothing]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(value)         => 1
    case NilLeaf             => 0
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value)         => value
    case NilLeaf             => 0
    case Branch(left, right) => Math.max(maximum(left), maximum(right))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(value)         => 1
    case NilLeaf             => 0
    case Branch(left, right) => 1 + Math.max(depth(left), depth(right))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case NilLeaf             => NilLeaf
  }

  def fold[A, B](t: Tree[A])(f: A => B)(nl: () => B)(b: (B, B) => B): B = t match {
    case Leaf(a)      => f(a)
    case NilLeaf      => nl()
    case Branch(l, r) => b(fold(l)(f)(nl)(b), fold(r)(f)(nl)(b))
  }

  def sizeByFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(() => 0)((l, r) => 1 + l + r)
}
