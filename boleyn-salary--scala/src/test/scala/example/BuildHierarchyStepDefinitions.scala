package example

import cucumber.api.scala.{ScalaDsl, EN}
import org.scalatest._

class BuildHierarchyStepDefinitions extends ScalaDsl with EN with Matchers {

  import example.PyramidBuilder._

  private var context: Map[String,String] = Map()

  Given("""^"([^"]+)" is superior of "([^"]+)"$""") { (superior: String, member: String) =>
    context += ("member" -> member, "superior" -> superior)
  }

  Then("""^I see the structure: (.*)$""") { (structure: String) =>
    val member = context("member").toInt
    val superior = context("superior").toInt

    buildHierarchy(member, superior).toString shouldEqual structure
  }

}