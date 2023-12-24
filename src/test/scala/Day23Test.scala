package me.astynax

import Day23._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day23Test extends AnyFunSuiteLike {
  private lazy val example = decode(
    """#.#####################
      |#.......#########...###
      |#######.#########.#.###
      |###.....#.>.>.###.#.###
      |###v#####.#v#.###.#.###
      |###.>...#.#.#.....#...#
      |###v###.#.#.#########.#
      |###...#.#.#.......#...#
      |#####.#.#.#######.#.###
      |#.....#.#.#.......#...#
      |#.#####.#.#.#########v#
      |#.#...#...#...###...>.#
      |#.#.#v#######v###.###v#
      |#...#.>.#...>.>.#.###.#
      |#####v#.#.###v#.#.###.#
      |#.....#...#...#.#.#...#
      |#.#########.###.#.#.###
      |#...###...#...#...#.###
      |###.###.#.###v#####v###
      |#...#...#.#.>.>.#.>.###
      |#.###.###.#.###.#.#v###
      |#.....###...###...#...#
      |#####################.#
      |""".stripMargin.lines().toScala(List)
  )

  test("decoding") {
    assert(example.map.nonEmpty)
    assert(input.map.nonEmpty)
  }

  test("step1 on example") {
    assert(step1(example) == 94)
  }

  test("step1 on input") {
    assert(step1(input) == 2162)
  }

  test("step2 on example") {
    assert(step2(example) == 154)
  }

  test("step2 on input") {
    assert(step2(input) == 6334)
  }
}
