package example

object Game {

  def score(rolls: List[Int]): Int = rolls match {
    case r1 :: r2 :: r3 :: tail if isStrike(r1) => 10 + r2 + r3 + score(r2 :: r3 :: tail)
    case r1 :: r2 :: r3 :: tail if isSpare(r1, r2) => 10 + r3 + score(r3 :: tail)
    case r :: tail => r + score(tail)
    case _ => 0
  }

  private def isStrike(roll: Int) = roll == 10


  private def isSpare(roll1: Int, roll2: Int) = roll1 + roll2 == 10

}
