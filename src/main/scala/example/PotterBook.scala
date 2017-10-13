package example

object PotterBook {

  val UNIT_PRICE = 8
  val DISTINCT_BOOK_DISCOUNTS = List(0, 0.05, 0.1, 0.2, 0.25)

  def price(bookIds: List[Int]): Double = {
    if (bookIds.size == 0) return 0

    val (uniqueIds, remainings) = findUniqueBooks(bookIds)
    val amount = uniqueIds.size * discountedUnitPrice(uniqueIds.size)
    amount + price(remainings)
  }

  private def discountedUnitPrice(distinctBooks: Int) = UNIT_PRICE * (1 - DISTINCT_BOOK_DISCOUNTS(distinctBooks - 1))

  private def findUniqueBooks(bookIds: List[Int]): (List[Int], List[Int]) = bookIds match {
    case b :: Nil => (List(b), List())
    case b :: tail =>
      val (uniqueIds, remainings) = findUniqueBooks(tail)
      if (uniqueIds.contains(b)) (uniqueIds, b :: remainings) else (b :: uniqueIds, remainings)
  }

}
