package example

import org.scalatest._

class PokerSpec extends FlatSpec with Matchers {

  val WEAKEST_HAND = "2C 3C 4C 5C 6S"

  it should "win with the strongest Straight flush" in {
    Poker.play("TD JD QD KD AD", WEAKEST_HAND) shouldEqual "Black wins. - with straight flush: Ace"
  }

  it should "win with the 2nd strongest Straight Flush" in {
    Poker.play("9D TD JD QD KD", WEAKEST_HAND) shouldEqual "Black wins. - with straight flush: King"
  }

  it should "win with the 3rd strongest Straight Flush" in {
    Poker.play("8D 9D TD JD QD", WEAKEST_HAND) shouldEqual "Black wins. - with straight flush: Queen"
  }

  it should "win with the 5th strongest Straight Flush" in {
    Poker.play("6D 7D 8D 9D TD", WEAKEST_HAND) shouldEqual "Black wins. - with straight flush: 10"
  }

  it should "win with the weakest Straight Flush" in {
    Poker.play("2D 3D 4D 5D 6D", WEAKEST_HAND) shouldEqual "Black wins. - with straight flush: 6"
  }

  it should "win with the strongest Four of a kind" in {
    Poker.play("KC AC AD AH AS", WEAKEST_HAND) shouldEqual "Black wins. - with four of a kind: Ace"
  }

  it should "win with the 2nd strongest Four of a Kind" in {
    Poker.play("KC KD KH KS AS", WEAKEST_HAND) shouldEqual "Black wins. - with four of a kind: King"
  }

  it should "win with the weakest Four of a kind" in {
    Poker.play("2C 2D 2H 2S 3S", WEAKEST_HAND) shouldEqual "Black wins. - with four of a kind: 2"
  }

  it should "win with the strongest Full House" in {
    Poker.play("AC AD AH KC KS", WEAKEST_HAND) shouldEqual "Black wins. - with full house: Ace"
  }

  it should "win with so so Full House" in {
    Poker.play("8C 8D 8H KC KS", WEAKEST_HAND) shouldEqual "Black wins. - with full house: 8"
  }

  it should "win with the strongest Flush" in {
    Poker.play("9D TD JD QD AD", WEAKEST_HAND) shouldEqual "Black wins. - with flush: Ace"
  }

  it should "win with the weakest Flush" in {
    Poker.play("2D 3D 4D 5D 7D", WEAKEST_HAND) shouldEqual "Black wins. - with flush: 7"
  }

  it should "win with the strongest Straight" in {
    Poker.play("TD JD QD KD AH", WEAKEST_HAND) shouldEqual "Black wins. - with straight: Ace"
  }

  it should "win with the strongest Three of a Kind" in {
    Poker.play("AD AH AC QD KD", WEAKEST_HAND) shouldEqual "Black wins. - with three of a kind: Ace"
  }

  it should "win with the strongest Two Pairs" in {
    Poker.play("QD KC KD AC AD", WEAKEST_HAND) shouldEqual "Black wins. - with two pairs: Ace"
  }

  //  it should "tie with the strongest hand" in {
//    Poker.play("TD JD QD KD AD", "TH JH QH KH AH") shouldEqual "Tie"
//  }

}
