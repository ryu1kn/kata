package example

object Util {
  def forallNeighbours[A](list: List[A], f: (A, A) => Boolean): Boolean =
    list.zipWithIndex.forall {
      case (_, 0) => true
      case (elem, index) => f(list(index - 1), elem)
    }

  def compareHeadToTail[A <: Ordered[A]](list1: List[A], list2: List[A]): Int =
    list1.zip(list2).foldLeft(0) {
      case (0, (elem1, elem2)) => elem1.compare(elem2)
      case (n, _) => n
    }

  def comparePosition[A](list: List[A], elem1: A, elem2: A): Int =
    list.indexOf(elem1).compare(list.indexOf(elem2))
}
