package fpinscala.datastructures

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class TreeTest extends AnyFunSuite {

  test("[3.25] testSize") {
    val root = Branch(Branch(Leaf(1), Leaf(2)), NilLeaf)
    Tree.size(root) should be(4)
  }

  test("[3.28] testMap") {
    val root = Branch(Branch(Leaf(1), Leaf(2)), NilLeaf)
    Tree.map(root)(_.toString) should be(Branch(Branch(Leaf("1"), Leaf("2")), NilLeaf))
  }

  test("[3.26] testMaximum") {
    val root = Branch(Branch(Leaf(1), Leaf(2)), NilLeaf)
    Tree.maximum(root) should be(2)
  }

  test("[3.27] testDepth") {
    val root = Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(2))), NilLeaf)
    Tree.depth(root) should be(4)
  }

  test("[3.29] testSizeByFold") {
    val root = Branch(Branch(Leaf(1), Leaf(2)), NilLeaf)
    Tree.sizeByFold(root) should be(4)
  }

}
