package underscore.cats.ch06

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Chapter06Test extends AnyFunSuite {

  test("[X.X] #1") {
    import cats.instances.option._
    import cats.syntax.apply._

    final case class Cat(name: String, born: Int, color: String);

    (
      Option("Garfield"),
      Option(1978),
      Option("Orange & black")
    ).mapN(Cat.apply) should be(Some(Cat("Garfield", 1978, "Orange & black")))
  }

  test("[X.X] #2") {
    import cats.Semigroupal
    Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _) should be(Some(6))
  }

  test("[X.X] #3") {
    import cats.Semigroupal
    Semigroupal[List].product(List(1, 2), List(3, 4)) should be(
      List((1, 3), (1, 4), (2, 3), (2, 4))
    )
  }

  test("[6.3] futures") {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent._
    import scala.concurrent.duration._

    import cats.Semigroupal
    import cats.instances.future._
    val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))
    Await.result(futurePair, 1.second) should be(("Hello", 123))
  }

  test("[6.4] parallel") {
    import cats.implicits._
    import cats.{Parallel, Semigroupal}

    type ErrorOr[A] = Either[Vector[String], A]
    val error1: ErrorOr[Int] = Left(Vector("Error 1"))
    val error2: ErrorOr[Int] = Left(Vector("Error 2"))
    val success1: ErrorOr[Int] = Right(123)
    Semigroupal[ErrorOr].product(error1, error2) should be(Left(Vector("Error 1")))
    Parallel[ErrorOr].parProductL(error1)(error2) should be(Left(Vector("Error 1", "Error 2")))
    Parallel[ErrorOr].parProductL(error1)(success1) should be(Left(Vector("Error 1")))

    // -- lists
    (List(1, 2), List(3, 4)).parTupled should be(List((1, 3), (2, 4)))
  }

}
