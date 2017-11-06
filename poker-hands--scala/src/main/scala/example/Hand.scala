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
    val cards = cardsStr.split(" ").toList.map(Card.create)
    HandRank.ranks.find(_.isOfRank(cards)) match {
      case Some(rank) => Hand(rank, cards)
    }
  }

  def lastCard(hand: Hand): Card = hand.cards.last

  def findMostCommonNumberCard(cards: List[Card]): Card = {
    val (_, mostCommonNumberCards) = cards
      .groupBy[Int](card => card.intValue)
      .maxBy { case (_, cardList) => cardList.size }
    mostCommonNumberCards.head
  }

  def hasSameNumberGroups(cards: List[Card], groupSizes: List[Int]): Boolean =
    cards
      .groupBy[Int](card => card.intValue)
      .map { case (_, cardList) => cardList.size }
      .toList
      .sorted match {
        case `groupSizes` => true
        case _ => false
      }
}
