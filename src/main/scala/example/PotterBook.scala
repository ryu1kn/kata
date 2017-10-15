package example

object PotterBook {

  type Book = Int

  val UNIT_PRICE = 8
  val DISTINCT_BOOK_DISCOUNTS = List(0, 0.05, 0.1, 0.2, 0.25)

  def price(bookIds: List[Book]): Double = {
    if (bookIds.size == 0) return 0

    val (uniqueIds, remainings) = findUniqueBooks(bookIds)
    subsetPrice(uniqueIds.size) + price(remainings)
  }

  private def subsetPrice(distinctBookCount: Int): Double = {
    val discountRate = DISTINCT_BOOK_DISCOUNTS(distinctBookCount - 1)
    UNIT_PRICE * (1 - discountRate) * distinctBookCount
  }

  private def findUniqueBooks(bookIds: List[Book]): (List[Book], List[Book]) = bookIds match {
    case List(b) => (List(b), List())
    case b :: tail =>
      val (uniqueIds, remainings) = findUniqueBooks(tail)
      if (uniqueIds.contains(b)) (uniqueIds, b :: remainings)
      else (b :: uniqueIds, remainings)
  }

}
