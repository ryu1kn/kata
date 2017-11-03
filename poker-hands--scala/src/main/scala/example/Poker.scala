package example

object Poker {

  import Hand._

  def play(hand1: String, hand2: String): String = {
    val hand = create(hand1)
    if (isStraightFlush(hand)) buildMessage("straight flush", lastCard(hand))
    else if (isFourOfAKind(hand)) buildMessage("four of a kind", findMostCommonNumberCard(hand))
    else if (isFullHouse(hand)) buildMessage("full house", findMostCommonNumberCard(hand))
    else if (isFlush(hand)) buildMessage("flush", lastCard(hand))
    else if (isStraight(hand)) buildMessage("straight", lastCard(hand))
    else if (isThreeOfAKind(hand)) buildMessage("three of a kind", findMostCommonNumberCard(hand))
    else buildMessage("two pairs", findMostCommonNumberCard(hand))
  }

  private def buildMessage(handName: String, card: Card): String =
    s"Black wins. - with $handName: ${card.valueName}"

}
