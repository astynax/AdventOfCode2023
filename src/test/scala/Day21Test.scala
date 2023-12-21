package me.astynax

import Day21._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day21Test extends AnyFunSuiteLike {
  private lazy val example = decode(
    """...........
      |.....###.#.
      |.###.##..#.
      |..#.#...#..
      |....#.#....
      |.##..S####.
      |.##..#...#.
      |.......##..
      |.##.#.####.
      |.##..##.##.
      |...........""".stripMargin.lines().toScala(List)
  )

  test("decoding") {
    assert(example.blocks.nonEmpty)
    assert(input.blocks.nonEmpty)
  }

  test("stop counting") {
    assert(countStopsAt(example, steps = 6) == 16)
    assert(countStopsAt(example, steps = 10) == 50)
    assert(countStopsAt(example, steps = 50) == 1594)
  }

  test("step1 on input") {
    assert(step1(input) == 3743)
  }

  test("step2 on input") {
    assert(step2(input) == 618261433219147L)
  }
}
