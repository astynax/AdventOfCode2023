package me.astynax

import scala.jdk.StreamConverters.StreamHasToScala

import org.scalatest.funsuite._
import Day03._

class Day03Test extends AnyFunSuiteLike {
  private val example: Input = decode(
    """|467..114..
       |...*......
       |..35..633.
       |......#...
       |617*......
       |.....+.58.
       |..592.....
       |......755.
       |...$.*....
       |.664.598..
       |""".stripMargin.lines().toScala(List))

  test("decoding") {
    assert(example.get(Pos(3, 2)) contains '5')
  }

  test("input decoding") {
    assert(input.nonEmpty)
  }

  test("locating of numbers") {
    assert(numberAt(example, Pos(6, 2)) contains 633)
    assert(numberAt(example, Pos(2, 4)) contains 617)
    assert(numberAt(example, Pos(7, 7)) contains 755)
  }

  test("step1 on example") {
    assert(step1(example) == 4361)
  }

  test("step1 on input") {
    assert(step1(input) == 537832)
  }

  test("step2 on example") {
    assert(step2(example) == 467835)
  }

  test("step2 on input") {
    assert(step2(input) == 81939900)
  }
}
