package io.ryuichi.common

object List {

  implicit class ListWrap[A](list: List[A]) {
    def maxsBy[B](evaluator: A => B)(implicit cmp: Ordering[B]): List[A] = {
      val max = list.map(evaluator).max
      list.filter(evaluator(_) == max)
    }
  }

}
