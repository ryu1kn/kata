package example

case class Hand(rank: HandRank, cards: List[Card]) extends Ordered[Hand] {
  override def compare(that: Hand): Int = (this, that) match {
    case (Hand(rank1, _), Hand(rank2, _)) if rank1 != rank2 => HandRank.compare(rank1, rank2)
    case (hand1@Hand(rank, _), hand2) => rank.compare(hand1, hand2)
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

  def strongestCommonCard(cards: List[Card]): Card =
    sameNumberGroups(cards).head match { case (_, card) => card }

  def sameNumberGroups(cards: List[Card]): List[(Int, Card)] =
    cards
      .groupBy[Int](card => card.intValue)
      .toList
      .map { case (_, cardList) => (cardList.size, cardList.head) }
      .sortBy[(Int, Int)] { case (groupSize, card) => (groupSize, card.intValue) }
      .reverse
}
