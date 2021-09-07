package mommifier.kotlin

object Mommifier {
    fun mommify(s: String): String =
        squashVowels(s).map(::mommifyCharacter).joinToString("")

    private fun squashVowels(s: String) = s.fold("") { acc, c ->
        when {
            acc.endsWithVowel() && isVowel(c) -> acc
            else -> "$acc$c"
        }
    }

    private fun String.endsWithVowel() = lastOrNull()?.let(::isVowel) ?: false

    private fun mommifyCharacter(c: Char) = if (isVowel(c)) "mommy" else c.toString()

    private fun isVowel(c: Char) = "aeiou".contains(c)
}
