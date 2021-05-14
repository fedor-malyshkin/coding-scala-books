package fpinscala.parallelism

import java.util.concurrent.{Executors, TimeUnit}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ParTest extends AnyFunSuite {

  test("[7.0] Par") {
    val parUnit = Par.unit {
      println("Hi from there, unit")
      1
    }
    val parLazyUnit = Par.lazyUnit {
      println("Hi from there, lazyUnit")
      1
    }
    val es12 = Executors.newFixedThreadPool(12)
    parLazyUnit(es12).get() should be(1)
  }

  test("[7.0] Par hanging") {
    val parLazyUnit = Par.lazyUnit {
      println("Hi from there, lazyUnit")
      1
    }
    val es1 = Executors.newFixedThreadPool(1)
    parLazyUnit(es1).get() should be(1)
  }

  ignore("[7.3] testMap2Timeout") {
    val v1 = Par.lazyUnit {
      Thread.sleep(2000)
      1
    }
    val v2 = Par.lazyUnit {
      Thread.sleep(2000)
      2
    }
    val res = Par.map2(v1, v2)(_ + _)
    val es12 = Executors.newFixedThreadPool(12)
    res(es12).get(1000, TimeUnit.MILLISECONDS) should be(3)
    val resTimeout = Par.map2Timeout(v1, v2)(_ + _)

    assertThrows[java.util.concurrent.TimeoutException] {
      resTimeout(es12).get(1000, TimeUnit.MILLISECONDS) should be(3)
    }
  }

  test("[7.4] asyncF") {
    val f = Par.asyncF((i: Int) => i + 1)
    val par = f(23)
    val es12 = Executors.newFixedThreadPool(12)
    par(es12).get() should be(24)
  }

  test("[7.X] sortPar") {
    val v1 = Par.lazyUnit(List(1, 2, 3, 4, 5, 0, 9, 8, 7, 6))
    val par = Par.sortPar(v1)
    val es12 = Executors.newFixedThreadPool(12)
    par(es12).get() should be(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  test("[7.5] parMap + sequence") {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val par = Par.parMap(l)(_ * 2)
    val es12 = Executors.newFixedThreadPool(12)
    par(es12).get() should be(List(2, 4, 6, 8, 10, 12, 14, 16, 18))

  }

  ignore("testDelay") {}

  ignore("testToParOps") {}

  ignore("testChoice") {}

}
