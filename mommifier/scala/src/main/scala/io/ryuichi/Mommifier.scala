package io.ryuichi

object Mommifier {

  def mommify(input: String): String =
    if (input.isEmpty || hasTooLessVowels(input)) input
    else replaceVowels(input)

  private def isVowel(c: Char) = "aeiou".contains(c)

  private def hasTooLessVowels(text: String) = getVowelRatio(text) <= 0.3

  private def getVowelRatio(text: String) = text.count(isVowel).toDouble / text.length

  private def replaceVowels(text: String): String = text
      .foldRight(List.empty[Char]) {
        case (c1, c2 :: rest) if isVowel(c1) && isVowel(c2) => c2 :: rest
        case (c, list) => c :: list
      }
      .flatMap(c => if (isVowel(c)) "mommy" else c.toString).mkString

}
