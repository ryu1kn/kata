package io.ryuichi

import org.scalatest._

class MommifierSpec extends WordSpec with Matchers {
  import Mommifier._

  "Mommifier" should {
    "return a given string" in {
      mommify("") shouldEqual ""
      mommify("b") shouldEqual "b"
    }

    "return mommy for vowels" in {
      mommify("a") shouldEqual "mommy"
      mommify("e") shouldEqual "mommy"
      mommify("i") shouldEqual "mommy"
      mommify("o") shouldEqual "mommy"
      mommify("u") shouldEqual "mommy"
    }

    "keep non-vowels as is" in {
      mommify("ab") shouldEqual "mommyb"
      mommify("ec") shouldEqual "mommyc"
    }

    "replace consecutive vowels into 1 mommy" in {
      mommify("aa") shouldEqual "mommy"
      mommify("ee") shouldEqual "mommy"
    }

    "keep non-vowels as is when replacing consecutive vowels" in {
      mommify("aab") shouldEqual "mommyb"
      mommify("aac") shouldEqual "mommyc"
      mommify("eec") shouldEqual "mommyc"
      mommify("cee") shouldEqual "cmommy"
    }

    "replace vowels only when they are more than 30% of the input" in {
      mommify("abbb") shouldEqual "abbb"
    }
  }
}
