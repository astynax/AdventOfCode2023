package me.astynax

import Day18._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day18Test extends AnyFunSuiteLike {
  private lazy val example: Input =
    """R 6 (#70c710)
      |D 5 (#0dc571)
      |L 2 (#5713f0)
      |D 2 (#d2c081)
      |R 2 (#59c680)
      |D 2 (#411b91)
      |L 5 (#8ceee2)
      |U 2 (#caa173)
      |L 1 (#1b58a2)
      |U 2 (#caa171)
      |R 2 (#7807d2)
      |U 3 (#a77fa3)
      |L 2 (#015232)
      |U 2 (#7a21e3)
      |""".stripMargin.lines().toScala(List).map(decode)

  test("decoding") {
    assert(example.nonEmpty)
    assert(input.nonEmpty)
  }

  test("step1 on example") {
    assert(step1(example) == 62)
  }

  test("step1 on input") {
    assert(step1(input) == 49061)
  }

  test("input 'fixing'") {
    assert(decode("R 6 (#70c710)").fixed == Item(
      'R', 461937, "",
    ))
  }

  test("step2 on example") {
    assert(step2(example) == 952408144115L)
  }

  test("step2 on input") {
    assert(step2(input) == 92556825427032L)
  }
}
