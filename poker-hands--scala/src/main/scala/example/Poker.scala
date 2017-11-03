package example

object Poker {

  import Hand._

  def play(hand1: String, hand2: String): String = {
    val hand = create(hand1)
    val cards = hand.cards
    if (isStraightFlush(cards)) buildMessage("straight flush", cards.last)
    else if (isFourOfAKind(cards)) buildMessage("four of a kind", findMostCommonNumberCard(cards))
    else if (isFullHouse(cards)) buildMessage("full house", findMostCommonNumberCard(cards))
    else if (isFlush(cards)) buildMessage("flush", cards.last)
    else if (isStraight(cards)) buildMessage("straight", cards.last)
    else if (isThreeOfAKind(cards)) buildMessage("three of a kind", findMostCommonNumberCard(cards))
    else buildMessage("two pairs", findMostCommonNumberCard(cards))
  }

  private def buildMessage(handName: String, card: Card): String =
    s"Black wins. - with $handName: ${card.valueName}"

}
