package underscore.cats.ch02

import cats.kernel.Monoid

case class Order(totalCost: Double, quantity: Double)

object Order {
  implicit def orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0, 0)
    override def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
}

object SuperAdder {

  import cats.syntax.semigroup._

  def add[A](items: List[A])(implicit ev: Monoid[A]): A =
    items.foldLeft(ev.empty)(_ |+| _)

  def addByContextBounds[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

}
