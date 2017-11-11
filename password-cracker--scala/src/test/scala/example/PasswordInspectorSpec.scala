package example

import org.scalatest._

class PasswordInspectorSpec extends FlatSpec with Matchers {

  import example.PasswordInspector.decompose

  it should "tell a password itself can be accepted" in {
    decompose("password", List("password")) shouldEqual Some(List("password"))
  }

  it should "use the 2nd password to build login attempt string" in {
    decompose("password2", List("password1", "password2")) shouldEqual Some(List("password2"))
  }

  it should "tell if login attempt cannot be built from the passwords" in {
    decompose("attempt", List("password")) shouldEqual None
  }

  it should "tell if login attempt string can be built from 2 passwords" in {
    decompose("attempt", List("att", "empt")) shouldEqual Some(List("att", "empt"))
  }

  it should "work even if 2 passwords are given in reverse order" in {
    decompose("attempt", List("empt", "att")) shouldEqual Some(List("att", "empt"))
  }

  it should "work even if 3 passwords are given in neither correct nor reverse order" in {
    decompose("attempt", List("at", "pt", "tem")) shouldEqual Some(List("at", "tem", "pt"))
  }

}
