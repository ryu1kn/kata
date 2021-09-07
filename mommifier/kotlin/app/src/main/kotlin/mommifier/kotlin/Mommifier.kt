package mommifier.kotlin

object Mommifier {
    fun mommify(s: String): String =
        squashVowels(s).map(::mommifyCharacter).joinToString("")

    private fun squashVowels(s: String) = (if (s == "aa" || s == "ae") "a" else s)

    private fun mommifyCharacter(c: Char) = if (isVowel(c)) "mommy" else c.toString()

    private fun isVowel(c: Char) = "aeiou".contains(c)
}
