package io.ryuichi

object ColouredTriangle {
  private val COLOURS = "RGB".toList

  def finalColour(colours: String): Char = reduce1Level(colours.toList)

  private def reduce1Level(colours: List[Char]): Char = colours match {
    case List(c)      => c
    case List(c1, c2) => reduce2Chars(c1, c2)
    case list =>
      reduce2Chars(reduce1Level(list.drop(1)), reduce1Level(list.dropRight(1)))
  }

  private def reduce2Chars(c1: Char, c2: Char): Char = (c1, c2) match {
    case (c1, c2) if c1 == c2 => c1
    case (c1, c2)             => COLOURS.find(c => c != c1 && c != c2).get
  }
}
