package example

import org.scalatest._
import PotterBook.price

class PotterBookSpec extends FlatSpec with Matchers {

  "Price" should "none if no books bought" in {
    price(List()) shouldEqual 0
  }

  "Discount" should "be none if bought only 1 book" in {
    price(List(0)) shouldEqual 8
  }

  "Discount" should "not be applied if bought 2 same books" in {
    price(List(0, 0)) shouldEqual 8 * 2
  }

  "Discount" should "be applied if bought 2 different books" in {
    price(List(0, 1)) shouldEqual 8 * 2 * (1 - 0.05)
  }

  "Discount" should "be applied if bought 3 different books" in {
    price(List(0, 1, 2)) shouldEqual 8 * 3 * (1 - 0.1)
  }

  "Discount" should "not be applied if bought 3 same books" in {
    price(List(0, 0, 0)) shouldEqual 8 * 3
  }

  "Discount" should "be applied only for 2 out of 3 books if only 2 books are different" in {
    price(List(0, 1, 0)) shouldEqual 8 * 2 * (1 - 0.05) + 8
  }

  "Discount" should "be applied if bought 4 different books" in {
    price(List(0, 1, 2, 3)) shouldEqual 8 * 4 * (1 - 0.2)
  }

  "Discount" should "not be applied if bought 4 same books" in {
    price(List(0, 0, 0, 0)) shouldEqual 8 * 4
  }

  "Discount" should "be applied only for 3 out of 4 books if only 3 books are different" in {
    price(List(0, 1, 0, 2)) shouldEqual 8 * 3 * (1 - 0.1) + 8
  }

  "Discount" should "be applied for 2 sets of 2 different books" in {
    price(List(0, 1, 0, 1)) shouldEqual 8 * 2 * (1 - 0.05) * 2
  }

  "Discount" should "be applied for only 2 different books of 4" in {
    price(List(0, 1, 0, 0)) shouldEqual 8 * 2 * (1 - 0.05) + 8 * 2
  }

  "Discount" should "be applied for 5 different books" in {
    price(List(0, 1, 2, 3, 4)) shouldEqual 8 * 5 * (1 - 0.25)
  }

  "Discount" should "not be applied if bought 5 same books" in {
    price(List(0, 0, 0, 0, 0)) shouldEqual 8 * 5
  }

  "Discount" should "be applied for 2 and 3 different books" in {
    price(List(0, 1, 0, 1, 2)) shouldEqual 8 * 2 * (1 - 0.05) + 8 * 3 * (1 - 0.1)
  }

}
