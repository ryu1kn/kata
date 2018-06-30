package io.ryuichi

import org.scalatest._
import ColouredTriangle.finalColour

class ColouredTriangleSpec extends FlatSpec with Matchers {

  it should "return Green" in {
    finalColour("G") shouldEqual 'G'
  }

  it should "return Green from 2 Greens" in {
    finalColour("GG") shouldEqual 'G'
  }

  it should "return Green from Blue and Red" in {
    finalColour("BR") shouldEqual 'G'
  }

  it should "return Green from 3 Greens" in {
    finalColour("GGG") shouldEqual 'G'
  }

  it should "return Red from Red on the edge" in {
    finalColour("GGR") shouldEqual 'R'
  }

  it should "return Green from 4 Greens" in {
    finalColour("GGGG") shouldEqual 'G'
  }

  it should "return Blue from Red on the edge" in {
    finalColour("GGGR") shouldEqual 'B'
  }

  it should "pass the sample problem" in {
    finalColour("RRGBRGBB") shouldEqual 'G'
  }
}
