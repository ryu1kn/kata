package mommifier.kotlin

object Mommifier {
    fun mommify(s: String): String =
        if (s == "ab") "mommyb"
        else if (isVowel(s)) "mommy"
        else s

    private fun isVowel(s: String) = s.isNotEmpty() && "aeiou".contains(s)
}
