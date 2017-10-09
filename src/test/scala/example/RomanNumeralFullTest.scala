package example

import argonaut._, Argonaut._
import org.scalatest.{FlatSpec, Matchers}

class RomanNumeralFullTest extends FlatSpec with Matchers {

  case class ArabicRomanPair(arabic: Int, roman: String)

  implicit def ExpectationDecodeJson: DecodeJson[ArabicRomanPair] =
    jdecode2L(ArabicRomanPair.apply)("arabic", "roman")

  val arabicRomanPairs = readJsonFile().decodeOption[List[ArabicRomanPair]]

  arabicRomanPairs match {
    case Some(arabicRomanPairs) => arabicRomanPairs.foreach {
      case ArabicRomanPair(arabic, roman) =>
        "it" should s"calculate all arabic-roman numeral pairs (${arabic})" in {
          RomanNumeralEncoder.encode(arabic) shouldEqual roman
        }
    }
  }

  def readJsonFile(): String = {
    val source = io.Source.fromFile("src/test/resources/roman-numerals-up-to-3999.json")
    try source.getLines().mkString
    finally source.close()
  }

}
