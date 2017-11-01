package example

import org.scalatest._

class GameSpec extends FlatSpec with Matchers {

  "Game" should "calculate a score for a gutter game" in {
    val rolls = List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    Game.score(rolls) shouldEqual 0
  }

  "Game" should "calculate a score for all 1 point game" in {
    val rolls = List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    Game.score(rolls) shouldEqual 20
  }

  "Game" should "calculate a score for one spare game" in {
    val rolls = List(4, 6, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    Game.score(rolls) shouldEqual 16
  }

  "Game" should "calculate a score for one strike game" in {
    val rolls = List(10, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    Game.score(rolls) shouldEqual 24
  }

  "Game" should "calculate a score for the 3rd throw in the tenth frame" in {
    val rolls = List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 7, 10)
    Game.score(rolls) shouldEqual 30
  }

}
