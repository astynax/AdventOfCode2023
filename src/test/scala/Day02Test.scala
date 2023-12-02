package me.astynax

import org.scalatest.funsuite.AnyFunSuiteLike
import Day02._

import scala.jdk.StreamConverters.StreamHasToScala

class Day02Test extends AnyFunSuiteLike {

  test("parsing") {
    assert(
      parseGame("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red") == (
        3, List(Cubes(20, 8, 6), Cubes(4, 13, 5), Cubes(1, 5, 0))
      )
    )
  }

  test("input parsing") {
    assert(input.nonEmpty)
  }

  test("takes comparability") {
    assert(Cubes(3, 4, 5) <= Cubes(3, 4, 5))
    assert(!(Cubes(3, 4, 5) <= Cubes(2, 4, 5)))
  }

  private val example =
    """|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
       |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
       |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
       |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
       |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
       |""".stripMargin.lines().toScala(List).map(parseGame)

  test("step1 on example") {
    assert(step1(example) == 8)
  }

  test("step1 on input") {
    assert(step1(input) == 2085)
  }

  test("step2 on example") {
    assert(step2(example) == 2286)
  }

  test("step2 on input") {
    assert(step2(input) == 79315)
  }
}
