package io.ryuichi

case class Reachable(nodes: List[Tree.Node],
                     edges: List[Tree.Edge],
                     radius: Int)

object Reachable {

  type Node = Tree.Node
  type Edge = Tree.Edge

  def create(edges: List[Edge], radius: Int): Reachable =
    Reachable(collectNodes(edges), edges, radius + 1)

  def pick(r: Reachable, node: Node): Reachable =
    Reachable(r.nodes.filterNot(_ == node), r.edges, r.radius - 1)

  def adjacent(r: Reachable, node: Node): List[Node] =
    r.edges
      .collect {
        case (`node`, adjacentNode) => adjacentNode
        case (adjacentNode, `node`) => adjacentNode
      }
      .filter(r.nodes.contains(_))

  private def collectNodes(edges: List[Edge]): List[Node] =
    edges
      .foldLeft(Set[Node]()) { (collected: Set[Node], edge: Edge) =>
        edge match {
          case (node1, node2) => collected + (node1, node2)
        }
      }
      .toList
}
