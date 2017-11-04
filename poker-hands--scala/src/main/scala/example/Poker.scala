package example

object Poker {

  import Hand._

  def play(hand1: String, hand2: String): String = {
    create(hand1) match {
      case StraightFlush(hand) => buildMessage("straight flush", lastCard(hand))
      case FourOfAKind(hand) => buildMessage("four of a kind", findMostCommonNumberCard(hand))
      case FullHouse(hand) => buildMessage("full house", findMostCommonNumberCard(hand))
      case Flush(hand) => buildMessage("flush", lastCard(hand))
      case Straight(hand) => buildMessage("straight", lastCard(hand))
      case ThreeOfAKind(hand) => buildMessage("three of a kind", findMostCommonNumberCard(hand))
      case TwoPairs(hand) => buildMessage("two pairs", findMostCommonNumberCard(hand))
      case Pair(hand) => buildMessage("pair", findMostCommonNumberCard(hand))
      case hand => buildMessage("high card", lastCard(hand))
    }
  }

  private def buildMessage(handName: String, card: Card): String =
    s"Black wins. - with $handName: ${card.valueName}"

}
