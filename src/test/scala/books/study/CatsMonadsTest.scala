package cats.study

import cats.{Foldable, Monad}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class CatsMonadsTest extends AnyFlatSpec with Matchers {

    it should "test01" in {
        CatsMonads.test01 should be(List(1, 2, 3, 3, 4, 5))
    }

    it should "test02" in {
        CatsMonads.test02 should be((1 to 3).toList.flatMap(x => (4 to 5).map(y => (x, y))))
    }

    it should "testFlatMap" in {
        (1 to 3).toList.flatMap(l => List(l + 1)) should be(List(2, 3, 4))
    }

    it should "testFlatMapOption" in {
        import cats.implicits._

        List(Some(1), Some(1), None).fold(Some(0))(_ |+| _) should be(Some(2))
    }

    it should "testReaderMonad02" in {
        CatsMonads.testReaderMonad02 should be("Garf")
    }

    it should "testReaderMonad03" in {
        CatsMonads.testReaderMonad03 should be("Hello, Garf")
    }

    it should "testReaderMonad04" in {
        CatsMonads.testReaderMonad04 should be("Hello, Garfield. Have a nice bowl of lasagne.")
    }

/*
    it should "test Apply" in {
        import cats.implicits._
        val option2 = Option(1) |@| Option(2)
        val option3 = option2 |@| Option.empty[Int]

        val addArity2 = (a: Int, b: Int) ⇒ a + b
        val addArity3 = (a: Int, b: Int, c: Int) ⇒ a + b + c

        option2 map addArity2 should be(Some(3))
        option3 map addArity3 should be(None)

        option2 apWith Some(addArity2) should be(Some(3))
        option3 apWith Some(addArity3) should be(None)

        option2.tupled should be(Some((1, 2)))
        option3.tupled should be(None)
    }
*/

    it should "test flatten" in {
        Option(Option(1)).flatten should be(Option(1))
        Option(None).flatten should be(None)
        List(List(1), List(2, 3), List(List(1))).flatten should be(List(1, 2, 3, List(1)))
        import cats.implicits._
        Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) should be(List(1, 2,
            3, 4,
            1, 2))
    }

    it should "test monads traverse_" in {
        import cats.implicits._

        def parseInt(s: String): Option[Int] =
            Either.catchOnly[NumberFormatException](s.toInt).toOption

        Foldable[List].traverse_(List("1", "2", "3"))(parseInt) should be(Option(()))
        Foldable[List].traverse_(List("a", "b", "c"))(parseInt) should be(None)

        val FoldableListOption = Foldable[List].compose[Option]
        FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))) should be(10)
        FoldableListOption.fold(List(Option("1"), Option("2"), None, Option("3"))) should be("123")

        Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))) should be(List(1, 2, 3, 4, 5))
        Foldable[List].foldK(List(None, Option("two"), Option("three"))) should be(Some("two"))
    }

}
