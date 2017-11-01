package example

import org.scalatest._
import PotterBook.price

class SampleSpec extends FlatSpec with Matchers {

  "Basic" should "pass" in {
    price(List()) shouldEqual 0
    price(List(0)) shouldEqual 8
    price(List(1)) shouldEqual 8
    price(List(2)) shouldEqual 8
    price(List(3)) shouldEqual 8
    price(List(4)) shouldEqual 8
    price(List(0, 0)) shouldEqual 8 * 2
    price(List(1, 1, 1)) shouldEqual 8 * 3
  }

  "Simple discounts" should "be calculated" in {
    price(List(0, 1)) shouldEqual 8 * 2 * 0.95
    price(List(0, 2, 4)) shouldEqual 8 * 3 * 0.9
    price(List(0, 1, 2, 4)) shouldEqual 8 * 4 * 0.8
    price(List(0, 1, 2, 3, 4)) shouldEqual 8 * 5 * 0.75
  }

  "Several discounts" should "be calculated" in {
    price(List(0, 0, 1)) shouldEqual 8 + (8 * 2 * 0.95)
    price(List(0, 0, 1, 1)) shouldEqual 2 * (8 * 2 * 0.95)
    price(List(0, 0, 1, 2, 2, 3)) shouldEqual (8 * 4 * 0.8) + (8 * 2 * 0.95)
    price(List(0, 1, 1, 2, 3, 4)) shouldEqual 8 + (8 * 5 * 0.75)
  }

  "Edge cases" should "be considered" in {
    price(List(0, 0, 1, 1, 2, 2, 3, 4)) shouldEqual 2 * (8 * 4 * 0.8)
    price(List(
      0, 0, 0, 0, 0,
      1, 1, 1, 1, 1,
      2, 2, 2, 2,
      3, 3, 3, 3, 3,
      4, 4, 4, 4)) shouldEqual 3 * (8 * 5 * 0.75) + 2 * (8 * 4 * 0.8)
  }

}
