package mommifier.kotlin

object Mommifier {
    fun mommify(s: String): String = if (s == "a" || s == "e") "mommy" else s
}
