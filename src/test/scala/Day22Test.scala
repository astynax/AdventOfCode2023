package me.astynax

import Day22._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day22Test extends AnyFunSuiteLike {
  private lazy val example = decode(
    """1,0,1~1,2,1
      |0,0,2~2,0,2
      |0,2,3~2,2,3
      |0,0,4~0,2,4
      |2,0,5~2,2,5
      |0,1,6~2,1,6
      |1,1,8~1,1,9
      |""".stripMargin.lines().toScala(List)
  )

  test("decoding") {
    assert(example.nonEmpty)
    assert(input.nonEmpty)
  }

  test("my example") {
    val e = decode(
      """0,0,5~2,0,5
        |0,0,7~0,2,8
        |2,0,10~2,2,10
        |0,1,20~2,1,20
        |2,0,25~2,0,25
        |1,2,30~1,2,33
        |""".stripMargin.lines().toScala(List)
    )
    assert(step1(e) == 3)
  }

  test("step1 on example") {
    assert(step1(example) == 5)
  }

  test("step1 on input") {
    assert(step1(input) == 430)
  }

  test("step2 on example") {
    assert(step2_(example) == 7)
  }

  test("step2 on input") {
    assert(step2_(input) == 60558)
  }
}
