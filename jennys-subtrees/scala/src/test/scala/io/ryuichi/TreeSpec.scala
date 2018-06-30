package io.ryuichi

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {

  it should "always find only 1 subtree if radius is 0" in {
    val edges = List((1, 2))
    val radius = 0
    Tree.countUniqueSubtree(edges, radius) shouldEqual 1
  }

  it should "find 1 subtree if there are only 2 nodes with radius 1" in {
    val edges = List((1, 2))
    val radius = 1
    Tree.countUniqueSubtree(edges, radius) shouldEqual 1
  }

  it should "find 2 subtrees" in {
    val edges = List((1, 2), (2, 3))
    val radius = 1
    Tree.countUniqueSubtree(edges, radius) shouldEqual 2
  }

  it should "pass sample problem 1" in {
    val edges = List(
      (1, 2),
      (1, 3),
      (1, 4),
      (1, 5),
      (2, 6),
      (2, 7)
    )
    val radius = 1
    Tree.countUniqueSubtree(edges, radius) shouldEqual 3
  }

  it should "pass sample problem 2" in {
    val edges = List(
      (1, 2),
      (2, 3),
      (3, 4),
      (4, 5),
      (5, 6),
      (6, 7)
    )
    val radius = 3
    Tree.countUniqueSubtree(edges, radius) shouldEqual 4
  }
}
