package example

import Math.{min, max}

object Minesweeper {

  private val DIGIT_CHAR_BASE = '0'.toInt

  def fillNumbers(board: List[List[Char]]): List[List[Char]] = {

    def getAdjacentTiles(i: Int, j: Int) =
      for {
        i <- max(i-1, 0) to min(i+1, board.size-1)
        j <- max(j-1, 0) to min(j+1, board(i).size-1)
      } yield board(i)(j)

    board.zipWithIndex.map {
      case (column, i) =>
        column.zipWithIndex.map {
          case ('.', j) => toDigit(getAdjacentTiles(i, j).count(_ == '*'))
          case ('*', _) => '*'
        }
    }
  }

  private def toDigit(number: Int): Char = (number + DIGIT_CHAR_BASE).toChar

}
