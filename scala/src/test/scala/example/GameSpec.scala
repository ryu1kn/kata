package example

import org.scalatest._

class GameSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    Game.greeting shouldEqual "hello"
  }
}
