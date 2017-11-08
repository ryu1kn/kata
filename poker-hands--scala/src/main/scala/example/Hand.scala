package example

case class Hand(rank: HandRank, cards: List[Card]) extends Ordered[Hand] {
  override def compare(that: Hand): Int = (this, that) match {
    case (hand1@Hand(rank1, _), hand2@Hand(rank2, _)) =>
      HandRank.compare(rank1, rank2) match {
        case 0 => rank1.compare(hand1, hand2)
        case x => x
      }
  }
}

object Hand {
  def create(cardsStr: String): Hand = {
    val sortedCards = cardsStr.split(" ").toList
      .map(Card.create)
      .sorted
    HandRank.ranks.find(_.isOfRank(sortedCards)) match {
      case Some(rank) => Hand(rank, sortedCards)
    }
  }

  def strongestCard(hand: Hand): Card = hand.cards.max

  def compare(hand1: Hand, hand2: Hand): Int =
    hand1.cards.zip(hand2.cards).foldRight(0) {
      case ((cardA, cardB), 0) => cardA.compare(cardB)
      case (_, n) => n
    }

  def findMostCommonNumberCard(cards: List[Card]): Card = {
    val (_, mostCommonNumberCards) = cards
      .groupBy[Int](card => card.intValue)
      .maxBy { case (_, cardList) => cardList.size }
    mostCommonNumberCards.head
  }

  def compareByGroupStrength(hand1: Hand, hand2: Hand): Int = {
    val hand1Group = sameNumberGroups(hand1.cards).map(_._2)
    val hand2Group = sameNumberGroups(hand2.cards).map(_._2)
    hand1Group.zip(hand2Group).foldLeft(0) {
      case (0, (cardA, cardB)) => cardA.compare(cardB)
      case (n, _) => n
    }
  }


  def sameNumberGroups(cards: List[Card]): List[(Int, Card)] =
    cards
      .groupBy[Int](card => card.intValue)
      .toList
      .map { case (cardValue, cardList) => (cardList.size, cardList.head) }
      .sortBy[(Int, Int)] { case (groupSize, card) => (groupSize, card.intValue) }
      .reverse
}
