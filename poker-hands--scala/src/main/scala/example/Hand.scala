package example

case class Hand(cards: List[Card])

object Hand {

  def create(hand: String): Hand = {
    val cards = hand.split(' ').toList.map { case Card(card) => card }
    Hand(cards)
  }

  def lastCard(hand: Hand): Card = hand.cards.last

  def isStraightFlush(hand: Hand): Boolean = satisfiesRelation(hand.cards, isSameSuiteIncrement)

  def isSameSuiteIncrement(card1: Card, card2: Card): Boolean =
    card2.suite == card1.suite && card2.intValue == card1.intValue + 1

  def isFourOfAKind(hand: Hand): Boolean = isNOfAKind(hand, 4)

  def findMostCommonNumberCard(hand: Hand): Card = {
    val (_, mostCommonNumberCards) = hand.cards
      .groupBy[Int](card => card.intValue)
      .maxBy { case (_, cardList) => cardList.size }
    mostCommonNumberCards.head
  }

  def isFullHouse(hand: Hand): Boolean =
    hand.cards
      .groupBy[Int](card => card.intValue)
      .map { case (_, cardList) => cardList.size }
      .toList
      .sorted match {
        case List(2, 3) => true
        case _ => false
      }

  def isFlush(hand: Hand): Boolean = hand.cards.groupBy[Char](card => card.suite).size == 1

  def isStraight(hand: Hand): Boolean =
    satisfiesRelation[Int](hand.cards.map(_.intValue).sorted, (card1, card2) => card2 == card1 + 1)

  def isThreeOfAKind(hand: Hand): Boolean = isNOfAKind(hand, 3)

  private def isNOfAKind(hand: Hand, n: Int): Boolean =
    hand.cards
      .groupBy[Int](card => card.intValue)
      .exists { case (_, cardList) => cardList.size == n }

  private def satisfiesRelation[A](list: List[A], f: (A, A) => Boolean): Boolean =
    list.zipWithIndex.forall {
      case (_, 0) => true
      case (el, index) => f(list(index - 1), el)
    }

}
