package example

import org.scalatest._

class GameSpec extends FlatSpec with Matchers {

  "Player" should "play a gutter game" in {
    val g = new Game()
    for (i <- 0 to 20) {
      g.roll(0)
    }
    g.score() shouldEqual 0
  }

}
