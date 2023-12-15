package me.astynax

import Day15._

import org.scalatest.funsuite.AnyFunSuiteLike

class Day15Test extends AnyFunSuiteLike {
  private lazy val example: Input = decode(
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
  )

  test("decoding") {
    assert(example.size == 11)
    assert(input.nonEmpty)
  }

  test("hashing") {
    assert(hashIt("HASH") == 52)
  }

  test("step1 on example") {
    assert(step1(example) == 1320)
  }

  test("step1 on input") {
    assert(step1(input) == 517551)
  }

  test("interpretation") {
    assert(interpret(example) == Map(
      0 -> List(
        "rn" -> 1,
        "cm" -> 2,
      ),
      1 -> List(),
      3 -> List(
        "ot" -> 7,
        "ab" -> 5,
        "pc" -> 6,
      )
    ))
  }

  test("step2 on example") {
    assert(step2(example) == 145)
  }

  test("step2 on input") {
    assert(step2(input) == 286097)
  }
}
