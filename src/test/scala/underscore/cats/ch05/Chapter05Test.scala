package underscore.cats.ch05

import java.util.concurrent.TimeUnit

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Chapter05Test extends AnyFunSuite {

  test("[X.X] #1") {
    import cats.data.OptionT
    import cats.instances.list._
    type ListOption[A] = OptionT[List, A]
    // for Monad
    import cats.syntax.applicative._ // for pure
    val result1: ListOption[Int] = OptionT(List(Option(10), Option(23)))
    val result2: ListOption[Int] = 32.pure[ListOption]

    val expected = OptionT(List(Option(42), Option(55)))
    result1.flatMap { (x: Int) =>
      result2.map { (y: Int) =>
        x + y
      }
    } should be(expected)

    val res = for {
      x <- result1
      y <- result2
    } yield x + y
    res should be(expected)

    // format: off
    (for (x <- result1; y <- result2) yield x + y) should be(expected)
    // format: on
  }

  test("[5.3.2] a Future of an Either of Option") {
    /*
- F[_] is the outer monad in the stack ( Either is the inner);
- E is the error type for the Either ;
- A is the result type for the Either .
     */
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future

    import cats.data.{EitherT, OptionT}
    import cats.implicits._

    type FutureEither[A] = EitherT[Future, String, A]
    type FutureEitherOption[A] = OptionT[FutureEither, A]
    val f: FutureEitherOption[Int] = 10.pure[FutureEitherOption]
    println(f)
    val interm1: FutureEither[Option[Int]] = f.value
    println(interm1)
    val interm2: Future[Either[String, Option[Int]]] = interm1.value
    println(interm2)
    val interm3: Either[String, Option[Int]] =
      Await.result(interm2, Duration.apply(5, TimeUnit.SECONDS))
    println(interm3)
    val interm4: Option[Int] = interm3.right.get
    println(interm4)
    val interm5: Int = interm4.get
    println(interm5)
  }

}
