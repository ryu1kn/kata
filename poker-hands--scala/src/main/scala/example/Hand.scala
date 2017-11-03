package example

case class Hand(cards: List[Card])

object Hand {

  def create(hand: String): Hand = {
    val cards = hand.split(' ').toList.map { case Card(card) => card }
    Hand(cards)
  }

  def isStraightFlush(cards: List[Card]): Boolean = satisfiesRelation(cards, isSameSuiteIncrement)

  def isSameSuiteIncrement(card1: Card, card2: Card): Boolean =
    card2.suite == card1.suite && card2.intValue == card1.intValue + 1

  def isFourOfAKind(cards: List[Card]): Boolean = isNOfAKind(cards, 4)

  def findMostCommonNumberCard(cards: List[Card]): Card = {
    val (_, mostCommonNumberCards) = cards
      .groupBy[Int](card => card.intValue)
      .maxBy { case (_, cardList) => cardList.size }
    mostCommonNumberCards(0)
  }

  def isFullHouse(cards: List[Card]): Boolean =
    cards
      .groupBy[Int](card => card.intValue)
      .map { case (_, cardList) => cardList.size }
      .toList
      .sorted match {
      case List(2, 3) => true
      case _ => false
    }

  def isFlush(cards: List[Card]): Boolean = cards.groupBy[Char](card => card.suite).size == 1

  def isStraight(cards: List[Card]): Boolean =
    satisfiesRelation[Int](cards.map(_.intValue).sorted, (card1, card2) => card2 == card1 + 1)

  def isThreeOfAKind(cards: List[Card]): Boolean = isNOfAKind(cards, 3)

  private def isNOfAKind(cards: List[Card], n: Int): Boolean =
    cards
      .groupBy[Int](card => card.intValue)
      .exists { case (_, cardList) => cardList.size == n }

  private def satisfiesRelation[A](list: List[A], f: (A, A) => Boolean): Boolean =
    list.zipWithIndex.forall {
      case (_, 0) => true
      case (el, index) => f(list(index - 1), el)
    }

}
