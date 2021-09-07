package mommifier.kotlin

object Mommifier {
    fun mommify(s: String): String =
        (if (s == "aa") "a" else s).map(::mommifyCharacter).joinToString("")

    private fun mommifyCharacter(c: Char) = if (isVowel(c)) "mommy" else c.toString()

    private fun isVowel(c: Char) = "aeiou".contains(c)
}
