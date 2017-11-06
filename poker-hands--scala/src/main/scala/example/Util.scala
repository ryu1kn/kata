package example

object Util {

  def forallNeighbours[A](list: List[A], f: (A, A) => Boolean): Boolean =
    list.zipWithIndex.forall {
      case (_, 0) => true
      case (el, index) => f(list(index - 1), el)
    }

}
