package mommifier.kotlin

object Mommifier {
    fun mommify(s: String): String =
        s.map { if (isVowel(it)) "mommy" else it.toString() }.joinToString("")

    private fun isVowel(s: Char) = "aeiou".contains(s)
}
