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

  it should "use the same password multiple times if necessary" in {
    decompose("pass", List("pa", "s")) shouldEqual Some(List("pa", "s", "s"))
  }

  it should "pass sample test #1" in {
    val passwords = List("because", "can", "do", "must", "we", "what")
    val expected = List("we", "do", "what", "we", "must", "because", "we", "can")
    decompose("wedowhatwemustbecausewecan", passwords) shouldEqual Some(expected)
  }

  it should "pass sample test #2" in {
    decompose("helloworld", List("hello", "planet")) shouldEqual None
  }

  it should "pass sample test #3" in {
    decompose("abcd", List("ab", "abcd", "cd")) shouldEqual Some(List("ab", "cd"))
  }

  it should "pass sample test #4" in {
    decompose("zfzahm", List("ozkxyhkcst", "xvglh", "hpdnb", "zfzahm")) shouldEqual Some(List("zfzahm"))
  }

  it should "pass sample test #5" in {
    decompose("gurwgrb", List("gurwgrb", "maqz", "holpkhqx", "aowypvopum")) shouldEqual Some(List("gurwgrb"))
  }

  it should "pass sample test #6" in {
    val passwords = List("a", "aa", "aaa", "aaaa", "aaaaa", "aaaaaa", "aaaaaaa", "aaaaaaaa", "aaaaaaaaa", "aaaaaaaaaa")
    decompose("aaaaaaaaaab", passwords) shouldEqual None
  }

}
