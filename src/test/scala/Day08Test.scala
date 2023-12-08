package me.astynax

import Day08._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day08Test extends AnyFunSuiteLike {
  test("decoding") {
    assert(input._2.nonEmpty)
  }

  private lazy val example: Input = e(
    """RL
      |
      |AAA = (BBB, CCC)
      |BBB = (DDD, EEE)
      |CCC = (ZZZ, GGG)
      |DDD = (DDD, DDD)
      |EEE = (EEE, EEE)
      |GGG = (GGG, GGG)
      |ZZZ = (ZZZ, ZZZ)
      |""".stripMargin
  )

  private def e(lines: String): Input = decode(lines.lines().toScala(List))

  test("step1 on examples") {
    assert(step1(example) == 2)
    assert(step1(e(
      """LLR
        |
        |AAA = (BBB, BBB)
        |BBB = (AAA, ZZZ)
        |ZZZ = (ZZZ, ZZZ)
        |""".stripMargin)) == 6
    )
  }

  test("step1 on input") {
    assert(step1(input) == 20777)
  }

  test("step2 on example") {
    assert(step2(e(
      """LR
        |
        |11A = (11B, XXX)
        |11B = (XXX, 11Z)
        |11Z = (11B, XXX)
        |22A = (22B, XXX)
        |22B = (22C, 22C)
        |22C = (22Z, 22Z)
        |22Z = (22B, 22B)
        |XXX = (XXX, XXX)
        |""".stripMargin)) == 6)
  }

  test("step2 on input") {
    assert(step2(input) == 13289612809129L)
  }
}
