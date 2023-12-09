package me.astynax

import Day09._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day09Test extends AnyFunSuiteLike {
  test("decoding") {
    assert(input.nonEmpty)
  }

  private val example: Input =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45
      |""".stripMargin.lines().toScala(List).map(decode)

  test("example decoding") {
    assert(example.nonEmpty)
  }

  test("diffs") {
    assert(diffs(example.head) == List(
      List(0, 3, 6, 9, 12, 15),
      List(3, 3, 3, 3, 3),
    ))
  }

  test("prediction the future") {
    assert(future(diffs(example.head)) == decode("0 3 6 9 12 15 18"))
  }

  test("step1 on example") {
    assert(step1(example) == 114)
  }

  test("step1 on input") {
    assert(step1(input) == 1974913025)
  }

  test("prediction the past") {
    assert(past(diffs(example.last)) == decode("5 10 13 16 21 30 45"))
  }

  test("step2 on example") {
    assert(step2(example) == 2)
  }

  test("step2 on input") {
    assert(step2(input) == 884)
  }
}
