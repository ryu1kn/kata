package example

object RomanNumeralEncoder {

  private case class RomanNumeral(letter: String, value: Int, delta: Option[RomanNumeral]) {

    def canPartlyRepresent(number: Int): Boolean = delta match {
      case Some(rn) => number >= value - rn.value
      case _        => number >= value
    }

    def biggestLteExpression(number: Int): (String, Int) = delta match {
      case _ if number >= value => (letter, value)
      case Some(rn)             => (rn.letter + letter, value - rn.value)
      case _                    => (letter, value)
    }

  }

  private val I = RomanNumeral("I", 1, None)
  private val V = RomanNumeral("V", 5, Option(I))
  private val X = RomanNumeral("X", 10, Option(I))
  private val L = RomanNumeral("L", 50, Option(X))
  private val C = RomanNumeral("C", 100, Option(X))
  private val D = RomanNumeral("D", 500, Option(C))
  private val M = RomanNumeral("M", 1000, Option(C))

  private val ROMAN_NUMERALS = List(M, D, C, L, X, V, I)

  def encode(n: Int): String = {
    if (n == 0) return ""

    val foundNumeral = ROMAN_NUMERALS.find(_.canPartlyRepresent(n))

    foundNumeral match {
      case Some(rn) =>
        val (letters, value) = rn.biggestLteExpression(n)
        letters + encode(n - value)
    }
  }

}
