package io.ryuichi

object MommifierFp extends Mommifier {

  override val mommify: String => String =
    TestSubject(_: String).fold(needsMommify, doMommify)

  private lazy val needsMommify =
    (text: String) => !text.isEmpty && getVowelRatio(text) > 0.3

  private lazy val isVowel = "aeiou".contains(_: Char)

  private lazy val getVowelRatio =
    (text: String) => text.count(isVowel).toDouble / text.length

  private lazy val doMommify = replaceVowels compose squashVowels

  private lazy val squashVowels =
    (_: String).foldRight(List.empty[Char]) {
      case (c1, c2 :: rest) if isVowel(c1) && isVowel(c2) => c2 :: rest
      case (c, list) => c :: list
    }
    .mkString

  private lazy val replaceVowels =
    (_: String).flatMap(c => if (isVowel(c)) "mommy" else c.toString)

}

// Are there any functions that already do this?
case class TestSubject[T](subject: T) {
  def fold(predicate: T => Boolean, convert: T => T): T =
    if (predicate(subject)) convert(subject)
    else subject
}
