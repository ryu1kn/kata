package io.ryuichi

import org.scalatest._

class MommifierSpec extends FlatSpec with Matchers {
  import Mommifier._

  it should "return an input" in {
    mommify("") shouldEqual ""
  }

}
