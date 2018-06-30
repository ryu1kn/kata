package example

case class CardValue(value: Int, longName: String) extends Ordered[CardValue] {
  override def compare(that: CardValue): Int = value.compare(that.value)
}

object CardValue {
  def create(symbol: Char): CardValue = symbol match {
    case 'A' => CardValue(14, "Ace")
    case 'K' => CardValue(13, "King")
    case 'Q' => CardValue(12, "Queen")
    case 'J' => CardValue(11, "Jack")
    case 'T' => CardValue(10, "10")
    case d   => CardValue(digitToInt(d), d.toString)
  }

  private def digitToInt(digit: Char): Int = digit - '0'.toInt
}

case class Card(suite: Char, cardValue: CardValue) extends Ordered[Card] {
  def valueName: String = cardValue.longName
  def intValue: Int = cardValue.value

  override def compare(that: Card): Int = cardValue.compare(that.cardValue)
}

object Card {
  def create(card: String): Card =
    Card(card.charAt(1), CardValue.create(card.charAt(0)))

  def isSameSuite(cardA: Card, cardB: Card): Boolean =
    cardA.suite == cardB.suite

  def isIncrementOf(cardA: Card, cardB: Card): Boolean =
    cardA.intValue + 1 == cardB.intValue

  def strongestCommonCard(cards: List[Card]): Card =
    sameNumberGroups(cards).head match { case (_, card) => card }

  def sameNumberGroups(cards: List[Card]): List[(Int, Card)] =
    cards
      .groupBy(card => card.intValue)
      .toList
      .map { case (_, cardList) => (cardList.size, cardList.head) }
      .sortBy[(Int, Int)] {
        case (groupSize, card) => (groupSize, card.intValue)
      }
      .reverse
}
