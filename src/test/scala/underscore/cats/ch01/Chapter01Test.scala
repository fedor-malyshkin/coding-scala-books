package underscore.cats.ch01

import java.time.LocalDateTime

import cats.kernel.Eq
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Chapter01Test extends AnyFunSuite {

  test("[book 1.4.2]") {
    import cats.Show
    import cats.instances.int._
    // for Show
    import cats.instances.string._ // for Show
    val showInt: Show[Int] = Show.apply[Int]
    showInt.show(123) should be("123")
    val showString: Show[String] = Show.apply[String]
  }

  test("[book 1.4.5]") {
    import cats.Show
    // for Show // for Show
    implicit val ldtShow: Show[LocalDateTime] = Show.show(ldt => s"${ldt.toString}")
    val ldt = LocalDateTime.of(1, 1, 1, 1, 1, 1)
    Show.apply[LocalDateTime].show(ldt) should be("0001-01-01T01:01:01")
  }

  test("[book 1.5]") {
    val res = List(1, 2, 3).map(Option(_)).filter(item => item == 1)
    res should be(List())
  }

  test("[book 1.5.2]") {
    import cats.kernel.Eq

    val intEq = Eq[Int]
    intEq.eqv(123, 123) should be(true)
    intEq.eqv(123, 123123) should be(false)

    Chapter01.compareInts() should be(true)

    assertDoesNotCompile("""
        |import cats.kernel.Eq
        |import cats.implicits._
        |(123 === ab)
        |""".stripMargin)
  }

  test("[book 1.5.3]") {
    import cats.implicits._

    (Some(1): Option[Int]) =!= None should be(true) // <--- look at this casting
    val res = List(1, 2, 3).map(Option(_)).filter(item => item =!= Some(1))
    res should be(List(Some(2), Some(3)))
  }

  test("[exercise 1.5.5]") {

    import cats.implicits._

    final case class Cat(name: String, age: Int, color: String)

    implicit val eqCats: Eq[Cat] =
      Eq.instance((c1, c2) => !(c1.name =!= c2.name || c1.age =!= c2.age || c1.color =!= c2.color))

    val cat1 = Cat("Garfield", 38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")
    val cat3 = Cat("Garfield", 38, "orange and black")
    cat1 =!= cat2 should be(true)
    cat1 =!= cat3 should be(false)

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]
    val optionCat3 = Option(cat3)

    optionCat1 =!= optionCat2 should be(true)
    optionCat1 =!= optionCat3 should be(false)
  }
}
