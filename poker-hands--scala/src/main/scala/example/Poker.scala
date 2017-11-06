package example

object Poker {

  case class Player(name: String, hand: Hand)

  object Player {
    def getWinner(player1: Player, player2: Player): Option[Player] =
      player1.hand.compare(player2.hand) match {
        case n if n > 0 => Some(player1)
        case n if n < 0 => Some(player2)
        case _ => None
      }
  }

  def play(hand1: String, hand2: String): String = {
    val player1 = Player("Black", Hand.create(hand1))
    val player2 = Player("White", Hand.create(hand2))

    Player.getWinner(player1, player2) match {
      case Some(Player(name, hand)) => buildMessage(name, hand.rank.name, hand.rank.decider(hand))
      case None => "Tie"
    }
  }

  private def buildMessage(winner: String, handName: String, card: Card): String =
    s"$winner wins. - with $handName: ${card.valueName}"

}
