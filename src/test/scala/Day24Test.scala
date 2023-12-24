package me.astynax

import Day24._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day24Test extends AnyFunSuiteLike {
  private lazy val example = decode(
    """19, 13, 30 @ -2,  1, -2
      |18, 19, 22 @ -1, -1, -2
      |20, 25, 34 @ -2, -2, -4
      |12, 31, 28 @ -1, -2, -1
      |20, 19, 15 @  1, -5, -3
      |""".stripMargin.lines().toScala(List)
  )

  test("decoding") {
    assert(example.nonEmpty)
    assert(example.head == Hailstone(19, 13, 30, -2, 1, -2))
    assert(input.nonEmpty)
  }
}
