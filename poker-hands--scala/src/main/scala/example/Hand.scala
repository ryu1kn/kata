package example

case class Hand(rank: HandRank, cards: List[Card]) extends Ordered[Hand] {
  override def compare(that: Hand): Int = (this, that) match {
    case (Hand(rank1, _), Hand(rank2, _)) if rank1 != rank2 => HandRank.compareRanks(rank1, rank2)
    case (hand1@Hand(rank, _), hand2) => rank.compare(hand1, hand2)
  }
}

object Hand {
  def create(cardsStr: String): Hand = {
    val sortedCards = cardsStr.split(" ").toList
      .map(Card.create)
      .sorted
    val rank = HandRank.findRankFor(sortedCards)
    Hand(rank, sortedCards)
  }
}
