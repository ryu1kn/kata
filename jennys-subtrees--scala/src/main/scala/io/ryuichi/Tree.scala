package io.ryuichi

case class Tree(node: Tree.Node, subTrees: List[Tree])

object Tree {

  type Node = Int
  type Edge = (Node, Node)

  def countUniqueSubtree(edges: List[Edge], radius: Int): Int = {
    val reachable = Reachable.create(edges, radius)
    val subTrees = buildTrees(reachable.nodes, reachable)
    collectUniqueStructures(subTrees).size
  }

  private def buildTrees(startNodes: List[Node],
                         reachable: Reachable): List[Tree] = {
    startNodes.map(node => buildTree(node, Reachable.pick(reachable, node)))
  }

  private def buildTree(start: Node, reachable: Reachable): Tree =
    if (reachable.radius == 0) {
      Tree(start, List())
    } else {
      val unvisitedNodes = Reachable.adjacent(reachable, start)
      val subTrees = buildTrees(unvisitedNodes, reachable)
      Tree(start, subTrees.sortBy(size _))
    }

  private def size(tree: Tree): Int =
    1 + tree.subTrees.foldLeft(0)((sum, subTree) => sum + size(subTree))

  private def collectUniqueStructures(trees: List[Tree]): List[Tree] =
    trees.foldLeft(List[Tree]()) { (trees, tree) =>
      if (trees.exists(isSameStructure(_, tree))) {
        trees
      } else {
        tree :: trees
      }
    }

  private def isSameStructure(t1: Tree, t2: Tree): Boolean = (t1, t2) match {
    case (Tree(_, trees1), Tree(_, trees2))
        if trees1.lengthCompare(trees2.size) == 0 =>
      trees1.zip(trees2).forall {
        case (subTree1, subTree2) => isSameStructure(subTree1, subTree2)
      }
    case _ => false
  }

}
