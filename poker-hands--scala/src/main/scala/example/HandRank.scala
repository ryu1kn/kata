package example

import example.ListUtil._
import Card._

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

  def findRankFor(cards: List[Card]): HandRank = ranks.find(_.isOfRank(cards)).get

  def compareRanks(rank1: HandRank, rank2: HandRank): Int = comparePosition(ranks, rank2, rank1)
}

sealed trait HandRank {
  protected val name: String

  protected def isOfRank(cards: List[Card]): Boolean

  def compare(handA: Hand, handB: Hand): Int = {
    val handAStrengths = sameNumberGroups(handA.cards).map(_._2)
    val handBStrengths = sameNumberGroups(handB.cards).map(_._2)
    compareHeadToTail(handAStrengths, handBStrengths)
  }

  def description(hand: Hand): String = s"$name: ${handDescription(hand)}"

  protected def handDescription(hand: Hand): String
}

case object StraightFlush extends HandRank {
  override val name: String = "straight flush"

  override def isOfRank(cards: List[Card]): Boolean = forallNeighbours(cards, isSameSuiteIncrement)

  private def isSameSuiteIncrement(card1: Card, card2: Card): Boolean =
    Card.isSameSuite(card1, card2) && Card.isIncrementOf(card1, card2)

  override def handDescription(hand: Hand): String = strongestCommonCard(hand.cards).valueName
}

case object FourOfAKind extends HandRank {
  override val name: String = "four of a kind"

  override def isOfRank(cards: List[Card]): Boolean = sameNumberGroups(cards) match {
    case List((4, _), _) => true
    case _ => false
  }

  override def handDescription(hand: Hand): String = strongestCommonCard(hand.cards).valueName
}

case object FullHouse extends HandRank {
  override val name: String = "full house"

  override def isOfRank(cards: List[Card]): Boolean = sameNumberGroups(cards) match {
    case List((3, _), (2, _)) => true
    case _ => false
  }

  override def handDescription(hand: Hand): String = sameNumberGroups(hand.cards) match {
    case List((3, card1), (2, card2)) => s"${card1.valueName} over ${card2.valueName}"
  }
}

case object Flush extends HandRank {
  override val name: String = "flush"

  override def isOfRank(cards: List[Card]): Boolean = forallNeighbours[Card](cards, Card.isSameSuite)

  override def handDescription(hand: Hand): String = strongestCommonCard(hand.cards).valueName
}

case object Straight extends HandRank {
  override val name: String = "straight"

  override def isOfRank(cards: List[Card]): Boolean = forallNeighbours[Card](cards, Card.isIncrementOf)

  override def handDescription(hand: Hand): String = strongestCommonCard(hand.cards).valueName
}

case object ThreeOfAKind extends HandRank {
  override val name: String = "three of a kind"

  override def isOfRank(cards: List[Card]): Boolean = sameNumberGroups(cards) match {
    case List((3, _), _*) => true
    case _ => false
  }

  override def handDescription(hand: Hand): String = strongestCommonCard(hand.cards).valueName
}

case object TwoPairs extends HandRank {
  override val name: String = "two pairs"

  override def isOfRank(cards: List[Card]): Boolean = sameNumberGroups(cards) match {
    case List((2, _), (2, _), _) => true
    case _ => false
  }

  override def handDescription(hand: Hand): String = strongestCommonCard(hand.cards).valueName
}

case object Pair extends HandRank {
  override val name: String = "pair"

  override def isOfRank(cards: List[Card]): Boolean = sameNumberGroups(cards) match {
    case List((2, _), _*) => true
    case _ => false
  }

  override def handDescription(hand: Hand): String = strongestCommonCard(hand.cards).valueName
}

case object HighCard extends HandRank {
  override val name: String = "high card"

  override def isOfRank(cards: List[Card]): Boolean = true

  override def handDescription(hand: Hand): String = strongestCommonCard(hand.cards).valueName
}
