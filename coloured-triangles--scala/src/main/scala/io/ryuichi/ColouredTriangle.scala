package io.ryuichi

object ColouredTriangle {
  private val COLOURS = "RGB".toList

  def finalColour(colours: String): Char = colours.toList match {
    case List(c)                  => c
    case List(c1, c2) if c1 == c2 => c1
    case List(c1, c2)             => findMissingCharacter(c1, c2)
  }

  private def findMissingCharacter(c1: Char, c2: Char) = {
    COLOURS.find(c => c != c1 && c != c2).get
  }
}
