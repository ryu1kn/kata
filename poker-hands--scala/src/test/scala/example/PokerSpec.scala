package example

import org.scalatest._

class PokerSpec extends FlatSpec with Matchers {

  it should "win with the strongest Straight flush" in {
    Poker.play("TD JD QD KD AD", "1H 2H 3H 4H 5H") shouldEqual "Black wins. - with straight flush: Ace"
  }

  it should "win with the 2nd strongest Straight Flush" in {
    Poker.play("9D TD JD QD KD", "1H 2H 3H 4H 5H") shouldEqual "Black wins. - with straight flush: King"
  }

  it should "win with the 3rd strongest Straight Flush" in {
    Poker.play("8D 9D TD JD QD", "1H 2H 3H 4H 5H") shouldEqual "Black wins. - with straight flush: Queen"
  }

  it should "win with the 5th strongest Straight Flush" in {
    Poker.play("6D 7D 8D 9D TD", "1H 2H 3H 4H 5H") shouldEqual "Black wins. - with straight flush: 10"
  }

  it should "win with the weakest Straight Flush" in {
    Poker.play("2D 3D 4D 5D 6D", "AC AD AH AS KS") shouldEqual "Black wins. - with straight flush: 6"
  }

  it should "win with the strongest Four of a kind" in {
    Poker.play("KC AC AD AH AS", "2C 2D 2H 2S 3S") shouldEqual "Black wins. - with four of a kind: Ace"
  }

  it should "win with the 2nd strongest Four of a kind" in {
    Poker.play("KC KD KH KS AS", "2C 2D 2H 2S 3S") shouldEqual "Black wins. - with four of a kind: King"
  }

  it should "win with the weakest Four of a kind" in {
    Poker.play("2C 2D 2H 2S 3S", "3C 3D 3H 4C 4S") shouldEqual "Black wins. - with four of a kind: 2"
  }

  it should "win with the strongest Full House" in {
    Poker.play("AC AD AH KC KS", "3C 3D 3H 4C 4S") shouldEqual "Black wins. - with full house: Ace"
  }

  it should "win with so so Full House" in {
    Poker.play("8C 8D 8H KC KS", "3C 3D 3H 4C 4S") shouldEqual "Black wins. - with full house: 8"
  }

  //  it should "tie with the strongest hand" in {
//    Poker.play("TD JD QD KD AD", "TH JH QH KH AH") shouldEqual "Tie"
//  }

}
