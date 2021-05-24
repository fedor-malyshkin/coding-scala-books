package underscore.cats.ch04

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Chapter04Test extends AnyFunSuite {

  test("[X.X] #1") {
    val listOfInts = List(1, 2, 3, 4)
    listOfInts.flatMap(l => List(l, l, l)) should be(List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4))
  }
  test("[X.X] map ot of flatMap") {

    trait Monad[F[_]] {
      def pure[A](a: A): F[A]
      def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
      def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(a => pure(func(a)))
    }
  }

  test("[4.2.3] sum of squares") {
    import cats.Monad
    import cats.syntax.flatMap._
    import cats.syntax.functor._ // for flatMap
    def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
      a.flatMap(x => b.map(y => x * x + y * y))
    import cats.instances.list._
    import cats.instances.option._
    // for Monad
    sumSquare(Option(3), Option(4)) should be(Some(25))
    sumSquare(List(1, 2, 3), List(4, 5)) should be(List(17, 26, 20, 29, 25, 34))
  }

  test("[4.4.2] either") {
    import cats.syntax.either._ // for asRight
    val a = 3.asRight[String]
    // a: Either[String, Int] = Right(3)
    val b = 4.asRight[String]
    // b: Either[String, Int] = Right(4)
    for {
      x <- a
      y <- b
    } yield x * x + y * y should be(25)

    val l: Either[Int, Nothing] = Left.apply(23)
    val r: Either[Nothing, Int] = Right.apply(23)

  }

  test("[4.6.4] eval trampolining") {
    import cats.Eval
    assertThrows[StackOverflowError] {
      def factorial(n: BigInt): Eval[BigInt] =
        if (n == 1) {
          Eval.now(n)
        } else {
          factorial(n - 1).map(_ * n)
        }
      factorial(500000).value
    }

    def factorial(n: BigInt): Eval[BigInt] =
      if (n == 1) {
        Eval.now(n)
      } else {
        Eval.defer(factorial(n - 1).map(_ * n))
      }

    factorial(5000).value

  }

  test("[4.7.3] use writer") {
    import cats.data.Writer
    import cats.instances.vector._
    import cats.syntax.applicative._
    import cats.syntax.writer._ // for Monoid

    type Logged[A] = Writer[Vector[String], A]

    def slowly[A](body: => A) =
      try body
      finally Thread.sleep(100)

    def factorial(n: Int): Logged[Int] =
      for {
        ans <-
          if (n == 0) {
            1.pure[Logged]
          } else {
            slowly(factorial(n - 1).map(_ * n))
          }
        _ <- Vector(s"fact $n $ans").tell
      } yield ans

    val (log, res) = factorial(5).run
    res should be(120)
    log should be(Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120"))
  }

  test("[4.8.1] use reader #1") {
    import cats.data.Reader
    final case class Cat(name: String, favoriteFood: String)
    val catName: Reader[Cat, String] =
      Reader(cat => cat.name)
    catName.run(Cat("Garfield", "lasagne")) should be("Garfield")
    val greetKitty: Reader[Cat, String] =
      catName.map(name => s"Hello ${name}")
    val feedKitty: Reader[Cat, String] =
      Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

    val greetAndFeed: Reader[Cat, String] =
      for {
        greet <- greetKitty
        feed <- feedKitty
      } yield s"$greet. $feed."
    greetAndFeed(Cat("Garfield", "lasagne")) should be(
      "Hello Garfield. Have a nice bowl of lasagne."
    )
  }

  test("[4.8.1] use reader - auth") {
    import cats.data.Reader
    import cats.syntax.applicative._
    final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
    )

    type DbReader[A] = Reader[Db, A]

    def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))

    def checkPassword(username: String, password: String): DbReader[Boolean] =
      Reader(db =>
        db.passwords
          .get(username)
          .contains(password)
      )

    def checkLogin(userId: Int, password: String): DbReader[Boolean] =
      for {
        userName <- findUsername(userId)
        res <- userName
          .map(userName => checkPassword(userName, password))
          .getOrElse(false.pure[DbReader])
      } yield res

    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )
    val db = Db(users, passwords)
    checkLogin(1, "zerocool").run(db) should be(true)
    checkLogin(4, "davinci").run(db) should be(false)
  }
}
