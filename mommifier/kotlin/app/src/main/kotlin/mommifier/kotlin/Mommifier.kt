package mommifier.kotlin

object Mommifier {
    fun mommify(s: String): String = if (shouldMommify(s)) "mommy" else s

    private fun shouldMommify(s: String) = s.isNotEmpty() && "aei".contains(s)
}
