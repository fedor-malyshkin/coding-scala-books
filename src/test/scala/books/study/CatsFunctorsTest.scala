package cats.study

import cats.study.CatsFunctors.Box
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class CatsFunctorsTest extends AnyFlatSpec with Matchers {

    it should "test01" in {
        CatsFunctors.test01 should be(List(5, 4))
    }

    it should "test02" in {
        CatsFunctors.test02 should be(Right(101))
    }

    it should "test03" in {
        CatsFunctors.test03 should be(List((1, 2), (2, 4), (3, 6)))
    }

    it should "test04" in {
        CatsFunctors.test04 should be(List(100, 200, 300, 4500))
    }

    it should "test05" in {
        CatsFunctors.test05 should be(List(2, 3, 4, 5, 6))
    }

    it should "test06" in {
        CatsFunctors.test06 should be(Box(124))
    }

}
