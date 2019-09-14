package io.ryuichi

import io.ryuichi.App._
import org.scalatest._

class AppSpec extends WordSpec with Matchers {

  "returns the same number" in {
    fizzbuzz(1) shouldEqual "1"
  }

  "returns the same number - 2" in {
    fizzbuzz(2) shouldEqual "2"
  }

  "returns Fizz if it's 3" in {
    fizzbuzz(3) shouldEqual "Fizz"
  }

  "returns Fizz if it's divisible by 3" in {
    fizzbuzz(6) shouldEqual "Fizz"
  }

  "returns Buzz if it's 5" in {
    fizzbuzz(5) shouldEqual "Buzz"
  }

  "returns Buzz if it's divisible by 5" in {
    fizzbuzz(10) shouldEqual "Buzz"
  }

  "returns FizzBuzz if it's 15" in {
    fizzbuzz(15) shouldEqual "FizzBuzz"
  }
}
