package fpinscala.errorhandling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.Try

class OptionTest extends AnyFunSuite {

  test("[4.1] test Option.map") {
    Some(1).map(_ + 1) should be(Some(2))
    None.map(_ => 1) should be(None)
  }

  test("[4.1] test Option.flatMap") {
    Some(1).flatMap(e => Some(e + 1)) should be(Some(2))
    Some(1).flatMap(e => None) should be(None)
    None.flatMap(e => Some(1)) should be(None)
  }

  test("[4.1] test Option.getOrElse") {
    Some(1).getOrElse(22) should be(1)
    None.getOrElse(22) should be(22)
  }

  test("[4.1] test Option.orElse") {
    Some(1).orElse(Some(22)) should be(Some(1))
    None.orElse(Some(22)) should be(Some(22))
  }

  test("[4.1] test Option.filter") {
    Some(1).filter(_ != 1) should be(None)
    Some(1).filter(_ == 1) should be(Some(1))
    None.filter(_ => true) should be(None)
    None.filter(_ => false) should be(None)
  }

  test("[4.2] test Option.variance") {
    val data = Seq(1.0, 2.0, 3.0)
    Option.variance(data) should be(Some(0.6666666666666666))
    Option.variance(Seq()) should be(None)
  }

  test("[4.3] test Option.map2") {
    Option.map2(Some(1), Some(2))(_ + _) should be(Some(3))
    Option.map2(Some(1), None)(_ + _) should be(None)
    Option.map2(None, Some(2))((a: Nothing, b) => 2) should be(None)
    Option.map2(None, None)((a: Nothing, b: Nothing) => 2) should be(None)

  }
  test("[4.4] test Option.sequence") {
    val data = List(Some(1), Some(2), Some(3))
    Option.sequence(data) should be(Some(List(3, 2, 1)))
    val dataWithNone = List(Some(1), Some(2), Some(3), None)
    Option.sequence(dataWithNone) should be(None)
  }

  test("[4.5] test Option.traverse") {
    val data = List(Some("1"), Some("2"), Some("3"))
    Option.traverse(data)(_.flatMap(s => Option.tryIt(s.toInt))) should be(Some(List(3, 2, 1)))
    val dataWithErr = List(Some("1"), Some("XXX"), Some("3"))
    Option.traverse(dataWithErr)(_.flatMap(s => Option.tryIt(s.toInt))) should be(None)
  }

}
