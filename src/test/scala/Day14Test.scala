package me.astynax

import Day14._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day14Test extends AnyFunSuiteLike {
  private lazy val example = e(
    """O....#....
      |O.OO#....#
      |.....##...
      |OO.#O....O
      |.O.....O#.
      |O.#..O.#.#
      |..O..#O..O
      |.......O..
      |#....###..
      |#OO..#....
      |""".stripMargin
  )

  private def e(s: String) = decode(
    s.lines().toScala(List)
  )

  test("decoding") {
    assert(example.platform.nonEmpty)
    assert(input.platform.nonEmpty)
  }

  test("tilting") {
    assert(example.tilt == e(
      """OOOO.#.O..
        |OO..#....#
        |OO..O##..O
        |O..#.OO...
        |........#.
        |..#....#.#
        |..O..#.O.O
        |..O.......
        |#....###..
        |#....#....
        |""".stripMargin
    ))

    assert(example.cycle == e(
      """.....#....
        |....#...O#
        |...OO##...
        |.OO#......
        |.....OOO#.
        |.O#...O#.#
        |....O#....
        |......OOOO
        |#...O###..
        |#..OO#....
        |""".stripMargin
    ))

    assert(example.cycle.cycle.cycle == e(
      """.....#....
        |....#...O#
        |.....##...
        |..O#......
        |.....OOO#.
        |.O#...O#.#
        |....O#...O
        |.......OOO
        |#...O###.O
        |#.OOO#...O
        |""".stripMargin
    ))
  }

  test("step1 on example") {
    assert(step1(example) == 136)
  }

  test("step1 on input") {
    assert(step1(input) == 109755)
  }

  test("step2 on example") {
    assert(step2(example) == 64)
  }

  test("step2 on input") {
    assert(step2(input) == 90928)
  }
}
