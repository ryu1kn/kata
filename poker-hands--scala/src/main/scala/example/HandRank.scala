package example

import example.Util.forallNeighbours

object HandRank {
  val ranks = List(
    StraightFlush,
    FourOfAKind,
    FullHouse,
    Flush,
    Straight,
    ThreeOfAKind,
    TwoPairs,
    Pair,
    HighCard
  )

  def compare(rank1: HandRank, rank2: HandRank): Int =
    ranks.indexOf(rank2).compare(ranks.indexOf(rank1))
}

sealed trait HandRank {

  val name: String

  def isOfRank(cards: List[Card]): Boolean

  def compare(hand: Hand, that: Hand): Int

  def decider(hand: Hand): Card

  protected def lastCard(hand: Hand): Card = hand.cards.last

  protected def findMostCommonNumberCard(cards: List[Card]): Card = {
    val (_, mostCommonNumberCards) = cards
      .groupBy[Int](card => card.intValue)
      .maxBy { case (_, cardList) => cardList.size }
    mostCommonNumberCards.head
  }

  protected def isNOfAKind(cards: List[Card], n: Int): Boolean =
    cards
      .groupBy[Int](card => card.intValue)
      .exists { case (_, cardList) => cardList.size == n }

  protected def hasSameNumberGroups(cards: List[Card], groupSizes: List[Int]): Boolean =
    cards
      .groupBy[Int](card => card.intValue)
      .map { case (_, cardList) => cardList.size }
      .toList
      .sorted match {
      case `groupSizes` => true
      case _ => false
    }

}

case object StraightFlush extends HandRank {
  override val name: String = "straight flush"

  override def isOfRank(cards: List[Card]): Boolean =
    forallNeighbours(cards, isSameSuiteIncrement)

  private def isSameSuiteIncrement(card1: Card, card2: Card): Boolean =
    card2.suite == card1.suite && card2.intValue == card1.intValue + 1

  def compare(hand1: Hand, hand2: Hand): Int = lastCard(hand1).compare(lastCard(hand2))

  override def decider(hand: Hand): Card = lastCard(hand)
}

case object FourOfAKind extends HandRank {
  override val name: String = "four of a kind"

  override def isOfRank(cards: List[Card]): Boolean = isNOfAKind(cards, 4)
  override def decider(hand: Hand): Card = findMostCommonNumberCard(hand.cards)

  def compare(hand1: Hand, hand2: Hand): Int =
    findMostCommonNumberCard(hand1.cards).compare(findMostCommonNumberCard(hand2.cards))
}

case object FullHouse extends HandRank {
  override val name: String = "full house"

  override def isOfRank(cards: List[Card]): Boolean = hasSameNumberGroups(cards, List(2, 3))
  override def decider(hand: Hand): Card = findMostCommonNumberCard(hand.cards)

  def compare(hand1: Hand, hand2: Hand): Int =
    findMostCommonNumberCard(hand1.cards).compare(findMostCommonNumberCard(hand2.cards))
}

case object Flush extends HandRank {
  override val name: String = "flush"

  override def isOfRank(cards: List[Card]): Boolean = cards.groupBy[Char](card => card.suite).size == 1
  override def decider(hand: Hand): Card = lastCard(hand)

  def compare(hand1: Hand, hand2: Hand): Int = lastCard(hand1).compare(lastCard(hand2))
}

case object Straight extends HandRank {
  override val name: String = "straight"

  override def isOfRank(cards: List[Card]): Boolean =
    forallNeighbours[Int](cards.map(_.intValue).sorted, (card1, card2) => card2 == card1 + 1)
  override def decider(hand: Hand): Card = lastCard(hand)
  def compare(hand1: Hand, hand2: Hand): Int = lastCard(hand1).compare(lastCard(hand2))
}

case object ThreeOfAKind extends HandRank {
  override val name: String = "three of a kind"

  override def isOfRank(cards: List[Card]): Boolean = isNOfAKind(cards, 3)
  override def decider(hand: Hand): Card = findMostCommonNumberCard(hand.cards)

  def compare(hand1: Hand, hand2: Hand): Int =
    findMostCommonNumberCard(hand1.cards).compare(findMostCommonNumberCard(hand2.cards))
}

case object TwoPairs extends HandRank {
  override val name: String = "two pairs"

  override def isOfRank(cards: List[Card]): Boolean = hasSameNumberGroups(cards, List(1, 2, 2))
  override def decider(hand: Hand): Card = findMostCommonNumberCard(hand.cards)

  def compare(hand1: Hand, hand2: Hand): Int =
    findMostCommonNumberCard(hand1.cards).compare(findMostCommonNumberCard(hand2.cards))
}

case object Pair extends HandRank {
  override val name: String = "pair"

  override def isOfRank(cards: List[Card]): Boolean = hasSameNumberGroups(cards, List(1, 1, 1, 2))
  override def decider(hand: Hand): Card = findMostCommonNumberCard(hand.cards)

  def compare(hand1: Hand, hand2: Hand): Int =
    findMostCommonNumberCard(hand1.cards).compare(findMostCommonNumberCard(hand2.cards))
}

case object HighCard extends HandRank {
  override val name: String = "high card"

  override def isOfRank(cards: List[Card]): Boolean = true
  override def decider(hand: Hand): Card = lastCard(hand)

  def compare(hand1: Hand, hand2: Hand): Int = lastCard(hand1).compare(lastCard(hand2))
}
