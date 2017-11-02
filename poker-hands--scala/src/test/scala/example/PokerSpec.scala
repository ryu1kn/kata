package example

import org.scalatest._

class PokerSpec extends FlatSpec with Matchers {

  it should "win with the strongest straight flush" in {
    Poker.play("TD JD QD KD AD", "1H 2H 3H 4H 5H") shouldEqual "Black wins. - with straight flush: Ace"
  }

  it should "win with the 2nd strongest straight flush" in {
    Poker.play("9D TD JD QD KD", "1H 2H 3H 4H 5H") shouldEqual "Black wins. - with straight flush: King"
  }

  it should "win with the 3rd strongest straight flush" in {
    Poker.play("8D 9D TD JD QD", "1H 2H 3H 4H 5H") shouldEqual "Black wins. - with straight flush: Queen"
  }

  it should "win with the 5th strongest straight flush" in {
    Poker.play("6D 7D 8D 9D TD", "1H 2H 3H 4H 5H") shouldEqual "Black wins. - with straight flush: 10"
  }

  it should "win with the weakest straight flush" in {
    Poker.play("2D 3D 4D 5D 6D", "AC AD AH AS KS") shouldEqual "Black wins. - with straight flush: 6"
  }

  it should "win with the strongest four of a kind" in {
    Poker.play("KC AC AD AH AS", "2C 2D 2H 2S 3S") shouldEqual "Black wins. - with four of a kind: Ace"
  }

//  it should "tie with the strongest hand" in {
//    Poker.play("TD JD QD KD AD", "TH JH QH KH AH") shouldEqual "Tie"
//  }

}
