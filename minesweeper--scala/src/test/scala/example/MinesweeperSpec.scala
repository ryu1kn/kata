package example

import org.scalatest._

class MinesweeperSpec extends FlatSpec with Matchers {

  it should "return 0 if no bombs around" in {
    Minesweeper.fillNumbers(List(List('.'))) shouldEqual List(List('0'))
  }

  it should "return * for bomb tile" in {
    Minesweeper.fillNumbers(List(List('*'))) shouldEqual List(List('*'))
  }

  it should "return 1 if the tile has 1 bomb right to it" in {
    Minesweeper.fillNumbers(List(List('.', '*'))) shouldEqual List(List('1', '*'))
  }

  it should "fill numbers with surrounded by bombs" in {
    val input = convertToBoard(List(
      "***",
      "*.*",
      "***",
    ))
    val expected = convertToBoard(List(
      "***",
      "*8*",
      "***",
    ))
    Minesweeper.fillNumbers(input) shouldEqual expected
  }


  it should "fill numbers for simple board" in {
    val input = convertToBoard(List(
      "*...",
      "....",
      ".*..",
      "...."
    ))
    val expected = convertToBoard(List(
      "*100",
      "2210",
      "1*10",
      "1110"
    ))
    Minesweeper.fillNumbers(input) shouldEqual expected
  }

  it should "fill numbers for non-square board" in {
    val input = convertToBoard(List(
        "**...",
        ".....",
        ".*..."
    ))
    val expected = convertToBoard(List(
        "**100",
        "33200",
        "1*100"
    ))
    Minesweeper.fillNumbers(input) shouldEqual expected
  }

  def convertToBoard(board: List[String]): List[List[Char]] = board.map(_.toList)

}
