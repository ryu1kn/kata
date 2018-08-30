package io.ryuichi

object Mommifier {

  def mommify(input: String): String =
    if (needsMommify(input)) doMommify(input)
    else input

  private def needsMommify(text: String) =
    !text.isEmpty && getVowelRatio(text) > 0.3

  private def isVowel(c: Char) = "aeiou".contains(c)

  private def getVowelRatio(text: String) = text.count(isVowel).toDouble / text.length

  private def doMommify(text: String) = replaceVowels(squashVowels(text))

  private def squashVowels(text: String): String = text
    .foldRight(List.empty[Char]) {
      case (c1, c2 :: rest) if isVowel(c1) && isVowel(c2) => c2 :: rest
      case (c, list) => c :: list
    }
    .mkString

  private def replaceVowels(text: String): String = text.flatMap(c => if (isVowel(c)) "mommy" else c.toString)

}
