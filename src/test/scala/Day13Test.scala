package me.astynax

import Day13._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day13Test extends AnyFunSuiteLike {
  private def e(s: String): Input = decode(s.lines().toScala(List))

  private lazy val example = e(
    """#.##..##.
      |..#.##.#.
      |##......#
      |##......#
      |..#.##.#.
      |..##..##.
      |#.#.##.#.
      |
      |#...##..#
      |#....#..#
      |..##..###
      |#####.##.
      |#####.##.
      |..##..###
      |#....#..#
      |""".stripMargin
  )

  test("decoding") {
    assert(example.nonEmpty)
    assert(input.nonEmpty)
  }

  test("mirroring") {
    assert(e(
      """.##..
        |.##..
        |""".stripMargin
    ).head.mh(0) contains 2)

    assert(e(
      """..##.
        |.#..#
        |""".stripMargin
    ).head.mh(0) contains 3)
  }

  test("example mirroring") {
    assert(example.head.mh(0) contains 5)
    assert(example(1).mh(0).isEmpty)
    assert(example.head.mv(0).isEmpty)
    assert(example(1).mv(0) contains 4)
  }

  test("step1 on examples") {
    assert(step1(example) == 405)
  }

  test("difficult cases") {
    assert(e(
      """####....####.#.##
        |.....#..#.#...#.#
        |#......##..#.###.
        |...####.#.##.#...
        |###...##.#..#.###
        |###..###.#..#.###
        |...####.#.##.#...
        |.#..#......#####.
        |.#..#......#####.
        |...####.#.##.#...
        |###..###.#..#.###
        |###...##.#..#.###
        |...####.#.##.#...
        |""".stripMargin
    ).head.mv(0) contains 8)

    assert(e(
      """...#..#
        |...#..#
        |##..##.
        |.#.##.#
        |..#..##
        |#.#.##.
        |#.#.###
        |##.##..
        |##.##..
        |#.#.###
        |..#.##.
        |..#..##
        |.#.##.#
        |##..##.
        |...#..#
        |""".stripMargin
    ).head.mv(0) contains 1)
  }

  test("step1 on input") {
    assert(step1(input) == 34100)
  }

  test("step2 on example") {
    assert(step2(example) == 400)
  }

  test("step2 on input") {
    assert(step2(input) == 33106)
  }
}
