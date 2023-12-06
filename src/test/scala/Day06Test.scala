package me.astynax

import org.scalatest.funsuite.AnyFunSuiteLike

import Day06._

class Day06Test extends AnyFunSuiteLike {
  test("input decoding") {
    assert(input.size == 4)
  }

  private val example: Input = List(
    7 -> 9,
    15 -> 40,
    30 -> 200,
  ).map { case (a, b) => BigInt(a) -> BigInt(b) }

  test("probing") {
    assert(probe(7, 9) == 4)
  }

  test("step1 on exampl") {
    assert(step1(example) == 288)
  }

  test("step1 on input") {
    assert(step1(input) == 0)
  }

  test("example joining") {
    assert(conjoin(example) == (71530, 940200))
  }

  test("step2 on example") {
    assert(step2(example) == 71503)
  }

  test("step2 on input") {
    assert(step2(input) == 39570185)
  }
}
