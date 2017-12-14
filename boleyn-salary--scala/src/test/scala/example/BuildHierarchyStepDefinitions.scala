package example

import cucumber.api.scala.{ScalaDsl, EN}
import org.scalatest._

class BuildHierarchyStepDefinitions extends ScalaDsl with EN with Matchers {

  import example.PyramidBuilder._

  private var member: Int = _
  private var superior: Int = _

  Given("""^"([^"]+)" is superior of "([^"]+)"$""") { (superior: String, member: String) =>
    this.member = member.toInt
    this.superior = superior.toInt
  }

  Then("""^I see the structure: (.*)$""") { (structure: String) =>
    buildHierarchy(member, superior).toString shouldEqual structure
  }

}