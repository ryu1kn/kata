package mommifier.kotlin

object Mommifier {
    fun mommify(s: String): String =
        s.map(::mommifyCharacter).joinToString("")

    private fun mommifyCharacter(it: Char) = if (isVowel(it)) "mommy" else it.toString()

    private fun isVowel(s: Char) = "aeiou".contains(s)
}
