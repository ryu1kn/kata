package example

case class Hand(rank: HandRank, cards: List[Card]) extends Ordered[Hand] {

  override def compare(that: Hand): Int = (this, that) match {
    case (Hand(rank1, _), Hand(rank2, _)) => HandRank.compare(rank1, rank2) match {
      case 0 => rank1.compare(this, that)
      case x => x
    }
  }

}

object Hand {

  def create(cardsStr: String): Hand = {
    val cards = cardsStr.split(" ").toList.map { case Card(card) => card }
    HandRank.ranks.find(_.isOfRank(cards)) match {
      case Some(rank) => Hand(rank, cards)
    }
  }

}
