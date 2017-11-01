package example

import org.scalatest._

class RomanNumeralEncoderTest extends FlatSpec with Matchers {

  val expectations = List(
    ( 1, "I"),
    ( 2, "II"),
    ( 3, "III"),
    ( 4, "IV"),
    ( 5, "V"),
    ( 6, "VI"),
    ( 7, "VII"),
    ( 8, "VIII"),
    ( 9, "IX"),
    (10, "X"),
    (11, "XI"),
    (12, "XII"),
    (14, "XIV"),
    (15, "XV"),
    (16, "XVI"),
    (19, "XIX"),
    (23, "XXIII"),
    (24, "XXIV"),
    (28, "XXVIII"),
    (29, "XXIX"),
    (39, "XXXIX"),
    (40, "XL"),
    (41, "XLI"),
    (48, "XLVIII"),
    (50, "L"),
    (51, "LI"),
    (88, "LXXXVIII"),
    (89, "LXXXIX"),
    (90, "XC"),
    (100, "C"),
    (198, "CXCVIII"),
    (400, "CD"),
    (500, "D"),
    (1000, "M"),
    (1889, "MDCCCLXXXIX"),
    (3999, "MMMCMXCIX")
  )

  expectations foreach {
    case (number, romanNumeral) =>
      "Encoder" should s"encode ${number}" in {
        RomanNumeralEncoder.encode(number) shouldEqual romanNumeral
      }
  }

}
