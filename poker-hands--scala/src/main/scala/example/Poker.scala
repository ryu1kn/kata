package example

object Poker {

  import Hand._

  def play(hand1: String, hand2: String): String = {
    (create(hand1), create(hand2)) match {
      case (StraightFlush(hand1), StraightFlush(hand2)) =>
        StraightFlush.compare(hand1, hand2) match {
          case n if n > 0 => buildMessage("Black", "straight flush", lastCard(hand1))
          case n if n < 0 => buildMessage("White", "straight flush", lastCard(hand2))
          case _ => "Tie"
        }
      case (StraightFlush(hand), _) => buildMessage("Black", "straight flush", lastCard(hand))
      case (_, StraightFlush(hand)) => buildMessage("White", "straight flush", lastCard(hand))
      case (FourOfAKind(hand), _) => buildMessage("Black", "four of a kind", findMostCommonNumberCard(hand))
      case (FullHouse(hand), _) => buildMessage("Black", "full house", findMostCommonNumberCard(hand))
      case (Flush(hand), _) => buildMessage("Black", "flush", lastCard(hand))
      case (Straight(hand), _) => buildMessage("Black", "straight", lastCard(hand))
      case (ThreeOfAKind(hand), _) => buildMessage("Black", "three of a kind", findMostCommonNumberCard(hand))
      case (TwoPairs(hand), _) => buildMessage("Black", "two pairs", findMostCommonNumberCard(hand))
      case (Pair(hand), _) => buildMessage("Black", "pair", findMostCommonNumberCard(hand))
      case (hand, _) => buildMessage("Black", "high card", lastCard(hand))
    }
  }

  private def buildMessage(winner: String, handName: String, card: Card): String =
    s"$winner wins. - with $handName: ${card.valueName}"

}
