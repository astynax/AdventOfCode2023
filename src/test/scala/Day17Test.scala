package me.astynax

import Day17._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day17Test extends AnyFunSuiteLike {
  private lazy val example = decode(
    """2413432311323
      |3215453535623
      |3255245654254
      |3446585845452
      |4546657867536
      |1438598798454
      |4457876987766
      |3637877979653
      |4654967986887
      |4564679986453
      |1224686865563
      |2546548887735
      |4322674655533
      |""".stripMargin.lines().toScala(List)
  )

  test("decoding") {
    assert(example.map.nonEmpty)
    assert(example.map(Pos(0, 0)) == 2)
    assert(example.width > 0)
    assert(example.height > 0)
    assert(example.map contains Pos(example.width - 1, example.height - 1))

    assert(input.map.nonEmpty)
  }

  test("step1 on example") {
    assert(step1(example) == 102)
  }

  test("step1 on input") {
    assert(step1(input) == 1001)
  }

  test("step2 on example") {
    assert(step2(example) == 94)
  }

  test("step2 on input") {
    assert(step2(input) == 1197)
  }
}
