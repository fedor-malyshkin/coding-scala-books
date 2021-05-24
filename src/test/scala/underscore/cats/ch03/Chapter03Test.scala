package underscore.cats.ch03

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Chapter03Test extends AnyFunSuite {

  test("[X.X] #1") {
    import cats.Functor
    import cats.instances.list._
    val list1 = List(1, 2, 3)
    val list2 = Functor[List].map(list1)(_ * 2)
    list2 should be(List(2, 4, 6))
  }

  test("[X.X] #2") {
    import cats.Functor

    val func = (x: Int) => x + 1
    val liftedFunc = Functor[Option].lift(func)
    liftedFunc(Option(1)) should be(Some(2))
  }

  test("[X.X] #3") {
    import cats.Functor
    val list1 = List(1, 2, 3)
    Functor[List].as(list1, "As") should be(List("As", "As", "As"))

    val optVal = Option(1)
    Functor[Option].as(optVal, "As") should be(Some("As"))
  }

  test("book [3.5.2]") {
    import cats.instances.function._
    import cats.syntax.functor._
    val func1 = (a: Int) => a + 1
    val func2 = (a: Int) => a * 2
    val func3 = (a: Int) => s"${a}!"
    val func4 = func1.map(func2).map(func3)
    func4(123) should be("248!")
  }

  test("book [3.5.2] doMath") {
    import cats.Functor
    import cats.syntax.functor._
    def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
      start.map(n => n + 1 * 2)
    doMath(Option(20)) should be(Some(22))
    doMath(List(1, 2, 3)) should be(List(3, 4, 5))
  }

  test("[3.6.1.1] test contrmap") {
    trait Printable[A] { self =>
      def format(value: A): String
      def contramap[B](func: B => A): Printable[B] =
        new Printable[B] {
          def format(value: B): String = self.format(func(value))
        }
    }

    def format[A](value: A)(implicit p: Printable[A]): String =
      p.format(value)

    implicit val booleanPrintable: Printable[Boolean] =
      new Printable[Boolean] {
        def format(value: Boolean): String =
          if (value) "yes" else "no"
      }

    implicit val intPrintable: Printable[Int] =
      new Printable[Int] {
        def format(value: Int): String =
          value.toString
      }

    format(true) should be("yes")

    final case class Box[A](value: A)

    implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
      new Printable[Box[A]] {
        def format(box: Box[A]): String = p.format(box.value)
      }
    val bBox = Box(true)
    val intBox = Box(12)

    format(bBox) should be("yes")
    format(intBox) should be("12")
  }

  test("[3.6.2] test imap") {
    trait Codec[A] { self =>
      def encode(value: A): String
      def decode(value: String): A
      def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
        override def encode(value: B): String = self.encode(enc.apply(value))
        override def decode(value: String): B = dec.apply(self.decode(value))
      }
    }
    def encode[A](value: A)(implicit c: Codec[A]): String =
      c.encode(value)
    def decode[A](value: String)(implicit c: Codec[A]): A =
      c.decode(value)

    implicit val stringCodec: Codec[String] =
      new Codec[String] {
        def encode(value: String): String = value
        def decode(value: String): String = value
      }

    implicit val intCodec: Codec[Int] =
      stringCodec.imap(_.toInt, _.toString)
    implicit val booleanCodec: Codec[Boolean] =
      stringCodec.imap(_.toBoolean, _.toString)
    implicit val doubleCodec: Codec[Double] =
      stringCodec.imap(_.toDouble, _.toString)

    decode[Double]("1.1") should be(1.1)
    encode(1.1) should be("1.1")

    final case class Box[A](value: A)
    /*
    classic way with rei-definition
implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = new Codec[Box[A]] {
      override def encode(value: Box[A]): String = c.encode(value.value)
      override def decode(value: String): Box[A] = Box(c.decode(value))
    }
     */

    implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
      c.imap[Box[A]](a => Box(a), b => b.value)

    decode[Box[Double]]("123.4") should be(Box(123.4))
    encode(Box(123.4)) should be("123.4")

  }
}
