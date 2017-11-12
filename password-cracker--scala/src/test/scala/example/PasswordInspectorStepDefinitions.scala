package example

import cucumber.api.scala.{ScalaDsl, EN}
import org.scalatest._
import scala.collection.mutable.Map

class PasswordInspectorStepDefinitions extends ScalaDsl with EN with Matchers {

  import example.PasswordInspector.decompose

  val context: Map[String,String] = Map()

  Given("""^the following passwords are registered in the system: (.*)$""") { passwords: String =>
    context.put("passwords", passwords)
  }

  When("""^I try to login with the password (.*)$""") { loginAttempt: String =>
    context.put("loginAttempt", loginAttempt)
  }

  Then("""^I should get the following passwords that can construct the password I entered: (.*)$""") { expected: String =>
    val passwords = context.get("passwords").get.split(", ").toList
    val loginAttempt = context.get("loginAttempt").get
    val expectedPasswords = expected.split(", ").toList

    decompose(loginAttempt, passwords) shouldEqual Some(expectedPasswords)
  }

  Then("""^I should be unable to get passwords that can construct the password I entered""") { () =>
    val passwords = context.get("passwords").get.split(", ").toList
    val loginAttempt = context.get("loginAttempt").get

    decompose(loginAttempt, passwords) shouldEqual None
  }

}
