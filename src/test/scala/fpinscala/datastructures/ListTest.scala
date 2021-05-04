package fpinscala.datastructures

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ListTest extends AnyFunSuite {

  test("testX") {
    List.x should be(3)
  }

  test("pattern matching") {
    val list = Cons("a", Cons("b", Cons("c", Nil)))
    val Cons(x, _) = list
    x should be("a")
    val unapplyRes = Cons.unapply(list)
    unapplyRes.get._1 should be("a")
    unapplyRes.get._2 should be(Cons("b", Cons("c", Nil)))
  }

  test("tail") {
    val list = Cons('a', Cons('b', Cons('c', Nil)))
    List.tail(list) should be(Cons('b', Cons('c', Nil)))
  }

  test("setHead") {
    val list = Cons('a', Cons('b', Cons('c', Nil)))
    List.setHead(list, 'A') should be(Cons('A', Cons('b', Cons('c', Nil))))
  }

  test("drop") {
    val list = Cons('a', Cons('b', Cons('c', Cons('d', Cons('e', Nil)))))
    List.drop(list, 3) should be(Cons('d', Cons('e', Nil)))
  }

  test("dropWhile") {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    List.dropWhile(list, (n: Int) => n < 4) should be(Cons(4, Cons(5, Nil)))
  }

  test("init") {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    List.init(list) should be(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
  }

  test("length by foldRight") {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    List.length(list) should be(5)
  }

  test("foldLeft") {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    val aggRightFunc = (e: Int, acc: String) => acc + e.toString
    val aggLeftFunc = (acc: String, e: Int) => acc + e.toString
    List.foldRight(list, "")(aggRightFunc) should be("54321")
    List.foldLeft(list, "")(aggLeftFunc) should be("12345")
  }

  test("reverse by foldLeft") {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    val start: List[Int] = Nil
    List.reverse(list) should be(Cons(5, Cons(4, Cons(3, Cons(2, Cons(1, Nil))))))
  }

  test("append by foldLeft") {
    val list = Cons(1, Cons(2, Cons(3, Nil)))
    val app = Cons(4, Cons(5, Nil))
    List.append(list, app) should be(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))
    List.append2(list, app) should be(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))
  }

  test("foldRight by foldLeft") {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    val aggRightFunc = (e: Int, acc: String) => acc + e.toString
    List.foldRight(list, "")(aggRightFunc) should be("54321")
    List.foldRight2(list, "")(aggRightFunc) should be("54321")
    List.foldRight3(list, "")(aggRightFunc) should be("54321")
  }

  test("[3.15] concat") {
    val list1 = Cons(1, Cons(2, Cons(3, Nil)))
    val list2 = Cons(1, Cons(2, Cons(3, Nil)))
    val list3 = Cons(1, Cons(2, Cons(3, Nil)))
    val listAgg: List[List[Int]] = Cons(list1, Cons(list2, Cons(list3, Nil)))
    List.concat(listAgg) should be(
      Cons(1, Cons(2, Cons(3, Cons(1, Cons(2, Cons(3, Cons(1, Cons(2, Cons(3, Nil)))))))))
    )
  }

  test("[3.16] +1") {
    val list = Cons(1, Cons(2, Cons(3, Nil)))
    List.addOne(list) should be(Cons(2, Cons(3, Cons(4, Nil))))
  }

  test("[3.17] List[Double] into a String") {
    val list = Cons(1.0, Cons(2.0, Cons(3.0, Nil)))
    List.doubleToString(list) should be(Cons("1.0", Cons("2.0", Cons("3.0", Nil))))
  }

  test("[3.18] map") {
    val list = Cons(1.0, Cons(2.0, Cons(3.0, Nil)))
    List.map(list)(_.toString) should be(Cons("1.0", Cons("2.0", Cons("3.0", Nil))))
  }

  test("[3.19] filter") {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    List.filter(list)(_ % 2 != 0) should be(Cons(1, Cons(3, Cons(5, Nil))))
    List.filter2(list)(_ % 2 != 0) should be(Cons(1, Cons(3, Cons(5, Nil))))
  }

  test("[3.20] flatMap") {
    val list = Cons(4, Cons(5, Nil))
    List.flatMap(list)(l => Cons(l, Cons(l, Nil))) should be(
      Cons(4, Cons(4, Cons(5, Cons(5, Nil))))
    )
  }

  test("[3.21] filter by flatMap") {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    List.filterByFlatMap(list)(_ % 2 != 0) should be(Cons(1, Cons(3, Cons(5, Nil))))
  }

  test("[3.22] zip") {
    val list1 = Cons(1, Cons(2, Nil))
    val list2 = Cons(4, Cons(5, Nil))
    List.zipInt(list1, list2) should be(Cons(5, Cons(7, Nil)))
  }

  test("[3.23] zipWith") {
    val list1 = Cons(1, Cons(2, Nil))
    val list2 = Cons(4, Cons(5, Nil))
    List.zipWith(list1, list2)(_ + _) should be(Cons(5, Cons(7, Nil)))
  }
}
