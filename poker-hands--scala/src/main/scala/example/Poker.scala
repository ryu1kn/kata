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
    else if (isFullHouse(cards)) buildMessage("full house", findMostCommonNumberCard(cards))
    else if (isFlush(cards)) buildMessage("flush", cards.last)
    else if (isStraight(cards)) buildMessage("straight", cards.last)
    else if (isThreeOfAKind(cards)) buildMessage("three of a kind", findMostCommonNumberCard(cards))
    else buildMessage("two pairs", findMostCommonNumberCard(cards))
  }

  private def isStraightFlush(cards: List[Card]): Boolean = satisfiesRelation(cards, isSameSuiteIncrement)

  private def isSameSuiteIncrement(card1: Card, card2: Card): Boolean =
    card2.suite == card1.suite && card2.intValue == card1.intValue + 1

  private def isFourOfAKind(cards: List[Card]): Boolean = isNOfAKind(cards, 4)

  private def findMostCommonNumberCard(cards: List[Card]): Card = {
    val (_, mostCommonNumberCards) = cards
      .groupBy[Int](card => card.intValue)
      .maxBy { case (_, cardList) => cardList.size }
    mostCommonNumberCards(0)
  }

  private def isFullHouse(cards: List[Card]): Boolean =
    cards
      .groupBy[Int](card => card.intValue)
      .map { case (_, cardList) => cardList.size }
      .toList
      .sorted match {
        case List(2, 3) => true
        case _ => false
      }

  private def isFlush(cards: List[Card]): Boolean = cards.groupBy[Char](card => card.suite).size == 1

  private def isStraight(cards: List[Card]): Boolean =
    satisfiesRelation[Int](cards.map(_.intValue).sorted, (card1, card2) => card2 == card1 + 1)

  private def isThreeOfAKind(cards: List[Card]): Boolean = isNOfAKind(cards, 3)

  private def isNOfAKind(cards: List[Card], n: Int): Boolean =
    cards
      .groupBy[Int](card => card.intValue)
      .exists { case (_, cardList) => cardList.size == n }

  private def satisfiesRelation[A](list: List[A], f: (A, A) => Boolean): Boolean =
    list.zipWithIndex.forall {
      case (el, 0) => true
      case (el, index) => f(list(index - 1), el)
    }

  private def buildMessage(handName: String, card: Card): String =
    s"Black wins. - with $handName: ${card.valueName}"

}
