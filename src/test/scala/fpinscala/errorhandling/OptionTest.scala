package fpinscala.errorhandling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

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

  test("test Option.variance") {}
  test("test Option.map2") {}
  test("test Option.sequence") {}
  test("test Option.traverse") {}

}
