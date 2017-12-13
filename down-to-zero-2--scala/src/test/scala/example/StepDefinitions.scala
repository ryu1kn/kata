package example

import cucumber.api.scala.{ScalaDsl, EN}
import org.scalatest._

class StepDefinitions extends ScalaDsl with EN with Matchers {

  import DownToZero2.getMinimumMove

  private var number: Int = -1

  Given("""^the number (\d+)$""") { (number: Int) =>
    this.number = number
  }

  Then("""^I get (\d+)$""") { (minimumMove: Int) =>
    getMinimumMove(number) shouldEqual minimumMove
  }

}
