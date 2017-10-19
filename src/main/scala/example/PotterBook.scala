package example

object PotterBook {

  type Book = Int

  val UNIT_PRICE = 8
  val DISTINCT_BOOK_DISCOUNTS = List(0, 0.05, 0.1, 0.2, 0.25)

  def price(books: List[Book]): Double = {
    if (books.size == 0) return 0

    val (uniqueBooks, remainingBooks) = findUniqueBooks(books)
    subsetPrice(uniqueBooks.size) + price(remainingBooks)
  }

  private def subsetPrice(distinctBookCount: Int): Double = {
    val discountRate = DISTINCT_BOOK_DISCOUNTS(distinctBookCount - 1)
    UNIT_PRICE * (1 - discountRate) * distinctBookCount
  }

  private def findUniqueBooks(books: List[Book]): (List[Book], List[Book]) =
    books.foldLeft((List[Book](), List[Book]())) {
      case ((uniqueBooks, remainingBooks), book) =>
        if (uniqueBooks.contains(book)) (uniqueBooks, book :: remainingBooks)
        else (book :: uniqueBooks, remainingBooks)
    }

}
