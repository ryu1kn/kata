package example

case class CardValue(value: Int, longName: String)

object CardValue {
  def create(symbol: Char): CardValue = symbol match {
    case 'A' => CardValue(14, "Ace")
    case 'K' => CardValue(13, "King")
    case 'Q' => CardValue(12, "Queen")
    case 'J' => CardValue(11, "Jack")
    case 'T' => CardValue(10, "10")
    case   d => CardValue(digitToInt(d), d.toString)
  }

  private def digitToInt(digit: Char): Int = digit - '0'.toInt
}

case class Card(suite: Char, cardValue: CardValue) extends Ordered[Card] {
  def valueName: String = cardValue.longName
  def intValue: Int = cardValue.value

  override def compare(that: Card): Int = intValue - that.intValue
}

object Card {
  def create(card: String): Card = Card(card.charAt(1), CardValue.create(card.charAt(0)))
}
