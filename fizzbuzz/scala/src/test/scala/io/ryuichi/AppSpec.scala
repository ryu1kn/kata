package io.ryuichi

import io.ryuichi.App._
import org.scalatest._

class AppSpec extends WordSpec with Matchers {

  "transport 1 person" in {
    transport(1, List()) shouldEqual 1
  }

}
