package me.astynax

import Day11._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day11Test extends AnyFunSuiteLike {
  test("input decoding") {
    assert(input.galaxies.nonEmpty)
  }

  private def e(s: String): Input = decode(s.lines().toScala(List))

  test("widening") {
    assert(
      Input.widen(0, Set(BPos(0, 0), BPos(1, 0), BPos(3, 0), BPos(6, 0))) == Set(
        BPos(0, 0), BPos(1, 0), BPos(4, 0), BPos(9, 0)
      )
    )
  }

  test("heightening") {
    assert(
     Input.heighten(
       0, Set(BPos(0, 0), BPos(0, 1), BPos(0, 3), BPos(0, 6))
     ) == Set(
       BPos(0, 0), BPos(0, 1), BPos(0, 4), BPos(0, 9)
     )
    )
  }

  test("expanding") {
    val e1 = e(
      """...#.
        |#....
        |.....
        |..#..
        |""".stripMargin
    )
    assert(e1.expand() == e(
      """....#.
        |#.....
        |......
        |......
        |...#..
        |""".stripMargin
    ))
  }

  private val example = e(
    """...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....
      |""".stripMargin
  )

  test("step1 on example") {
    assert(step1(example) == 374)
  }

  test("step1 on input") {
    assert(step1(input) == 9648398)
  }

  test("calculating with a mid scale") {
    assert(calculate(example, 10) == 1030)
    assert(calculate(example, 100) == 8410)
  }

  test("step2 on input") {
    assert(step2(input) == 618800410814L)
  }
}
