package io.ryuichi

object MommifierOo extends Mommifier {

  override val mommify: String => String = new Mommifier(_).mommify

  class Mommifier(text: String) {
    def mommify: String = if (needsMommify) mommified else text

    private def needsMommify: Boolean = !text.isEmpty && vowelRatio > 0.3

    private def vowelRatio = text.count(isVowel).toDouble / text.length

    private def mommified = replaceVowels(vowelsSquashed)

    private def vowelsSquashed: String = text
      .foldRight(List.empty[Char]) {
        case (c1, c2 :: rest) if isVowel(c1) && isVowel(c2) => c2 :: rest
        case (c, list) => c :: list
      }
      .mkString

    private def replaceVowels(text: String): String = text.flatMap(c => if (isVowel(c)) "mommy" else c.toString)

    private def isVowel(c: Char) = "aeiou".contains(c)
  }
}
