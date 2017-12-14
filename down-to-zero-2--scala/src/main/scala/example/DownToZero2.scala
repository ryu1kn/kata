package example

object DownToZero2 {

  def getMinimumMove(number: Int, moveCount: Int = 0): Int =
    if (number == 0) {
      moveCount
    } else {
      getMinimumMove(findNextNumber(number), moveCount + 1)
    }

  private def findNextNumber(number: Int): Int = {
    val pairs = findPairs(number)
    if (pairs.isEmpty) {
      number - 1
    } else {
      findCentrePair(pairs)
    }
  }

  private def findPairs(number: Int): List[(Int, Int)] =
    for {
      i <- (2 until number).toList if number % i == 0
    } yield (i, number / i)

  private def findCentrePair(pairs: List[(Int, Int)]): Int =
    pairs.minBy[Int] { case (n1, n2) => n1 + n2 } match {
      case (n1, n2) => Math.max(n1, n2)
    }
}
