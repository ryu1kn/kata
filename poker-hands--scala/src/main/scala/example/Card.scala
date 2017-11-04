package example

case class CardValue(value: Int, longName: String)

object CardValue {
  def unapply(symbol: Char): Option[CardValue] = symbol match {
    case 'A' => Some(CardValue(14, "Ace"))
    case 'K' => Some(CardValue(13, "King"))
    case 'Q' => Some(CardValue(12, "Queen"))
    case 'J' => Some(CardValue(11, "Jack"))
    case 'T' => Some(CardValue(10, "10"))
    case   d => Some(CardValue(digitToInt(d), d.toString))
  }

  private def digitToInt(digit: Char): Int = digit - '0'.toInt
}

case class Card(suite: Char, cardValue: CardValue) extends Ordered[Card] {
  def valueName: String = cardValue.longName
  def intValue: Int = cardValue.value

  override def compare(that: Card): Int = intValue - that.intValue
}

object Card {
  def unapply(card: String): Option[Card] = {
    val CardValue(value) = card.charAt(0)
    Some(Card(card.charAt(1), value))
  }
}
