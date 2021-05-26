import cats.Monoid
import cats.instances.int._

val opt1 = Option(1)
val opt2 = Option(23)

val t = Monoid.combine(opt1, opt2)
Monoid.isCommutative[Option[Int]]
Monoid.empty[Option[Int]]
Monoid.empty[Double]
Monoid[Boolean].empty()
