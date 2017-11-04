package example

case class Hand(cards: List[Card])

object Hand {

  trait HandRank {
    def unapply(hand: Hand): Option[Hand] = hand match {
      case h if isOfRank(h) => Some(h)
      case _ => None
    }

    protected def isOfRank(hand: Hand): Boolean
  }

  object StraightFlush extends HandRank {
    override def isOfRank(hand: Hand): Boolean =
      satisfiesRelation(hand.cards, isSameSuiteIncrement)

    private def isSameSuiteIncrement(card1: Card, card2: Card): Boolean =
      card2.suite == card1.suite && card2.intValue == card1.intValue + 1

    def compare(hand1: Hand, hand2: Hand): Int = lastCard(hand1).compare(lastCard(hand2))
  }

  object FourOfAKind extends HandRank {
    override def isOfRank(hand: Hand): Boolean = isNOfAKind(hand, 4)
  }

  object FullHouse extends HandRank {
    override def isOfRank(hand: Hand): Boolean = hasSameNumberGroups(hand, List(2, 3))
  }

  object Flush extends HandRank {
    override def isOfRank(hand: Hand): Boolean = hand.cards.groupBy[Char](card => card.suite).size == 1
  }

  object Straight extends HandRank {
    override def isOfRank(hand: Hand): Boolean =
      satisfiesRelation[Int](hand.cards.map(_.intValue).sorted, (card1, card2) => card2 == card1 + 1)
  }

  object ThreeOfAKind extends HandRank {
    override def isOfRank(hand: Hand): Boolean = isNOfAKind(hand, 3)
  }

  object TwoPairs extends HandRank {
    override def isOfRank(hand: Hand): Boolean = hasSameNumberGroups(hand, List(1, 2, 2))
  }

  object Pair extends HandRank {
    override def isOfRank(hand: Hand): Boolean = hasSameNumberGroups(hand, List(1, 1, 1, 2))
  }

  def create(hand: String): Hand = {
    val cards = hand.split(' ').toList.map { case Card(card) => card }
    Hand(cards)
  }

  def lastCard(hand: Hand): Card = hand.cards.last

  def findMostCommonNumberCard(hand: Hand): Card = {
    val (_, mostCommonNumberCards) = hand.cards
      .groupBy[Int](card => card.intValue)
      .maxBy { case (_, cardList) => cardList.size }
    mostCommonNumberCards.head
  }

  private def isNOfAKind(hand: Hand, n: Int): Boolean =
    hand.cards
      .groupBy[Int](card => card.intValue)
      .exists { case (_, cardList) => cardList.size == n }

  private def hasSameNumberGroups(hand: Hand, groupSizes: List[Int]): Boolean =
    hand.cards
      .groupBy[Int](card => card.intValue)
      .map { case (_, cardList) => cardList.size }
      .toList
      .sorted match {
        case `groupSizes` => true
        case _ => false
      }

  private def satisfiesRelation[A](list: List[A], f: (A, A) => Boolean): Boolean =
    list.zipWithIndex.forall {
      case (_, 0) => true
      case (el, index) => f(list(index - 1), el)
    }

}
