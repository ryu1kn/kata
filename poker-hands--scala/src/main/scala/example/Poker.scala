package example

object Poker {

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

  case class Card(suite: Char, cardValue: CardValue) {
    def valueName: String = cardValue.longName
    def intValue: Int = cardValue.value
  }

  object Card {
    def unapply(card: String): Option[Card] = {
      val CardValue(value) = card.charAt(0)
      Some(Card(card.charAt(1), value))
    }
  }

  def play(hand1: String, hand2: String): String = {
    val cards = hand1.split(' ').toList.map {
      case Card(card) => card
    }
    if (isStraightFlush(cards)) buildMessage("straight flush", cards.last)
    else if (isFourOfAKind(cards)) buildMessage("four of a kind", findMostCommonNumberCard(cards))
    else buildMessage("full house", findMostCommonNumberCard(cards))
  }

  private def isStraightFlush(cards: List[Card]): Boolean =
    cards.zipWithIndex.forall {
      case (_, 0) => true
      case (card, index) => isSameSuiteIncrement(cards(index - 1), card)
    }

  private def isSameSuiteIncrement(card1: Card, card2: Card): Boolean = {
    card2.suite == card1.suite && card2.intValue == card1.intValue + 1
  }

  private def isFourOfAKind(cards: List[Card]): Boolean =
    cards
      .groupBy[Int](card => card.intValue)
      .exists { case (_, cardList) => cardList.size == 4 }

  private def findMostCommonNumberCard(cards: List[Card]): Card = {
    val (_, mostCommonNumberCards) = cards
      .groupBy[Int](card => card.intValue)
      .maxBy { case (_, cardList) => cardList.size }
    mostCommonNumberCards(0)
  }

  private def buildMessage(handName: String, card: Card): String =
    s"Black wins. - with $handName: ${card.valueName}"

}
