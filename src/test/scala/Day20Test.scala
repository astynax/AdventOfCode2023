package me.astynax

import Day20._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day20Test extends AnyFunSuiteLike {
  def e(s: String): Input = decode(s.lines().toScala(List))

  private lazy val example1 = e(
    """broadcaster -> a, b, c
      |%a -> b
      |%b -> c
      |%c -> inv
      |&inv -> a
      |""".stripMargin
  )

  private lazy val example2 = e(
    """broadcaster -> a
      |%a -> inv, con
      |&inv -> b
      |%b -> con
      |&con -> output
      |""".stripMargin)

  test("decoding") {
    assert(example1.broadcastTo == List(
      Name("a"), Name("b"), Name("c"),
    ))
    assert(example1.modules == Map(
      Name("a") -> FF(List(Name("b"))),
      Name("b") -> FF(List(Name("c"))),
      Name("c") -> FF(List(Name("inv"))),
      Name("inv") -> NAND(List(Name("a")), Map(
        Name("c") -> Signal.Low
      )),
    ))

    assert(example2.modules.nonEmpty)
    assert(example2.modules(Name("con")).asInstanceOf[NAND].state.keys == Set(
      Name("a"), Name("b"),
    ))

    assert(input.modules.nonEmpty)
  }

  test("button pushing") {
    assert(pushButton(input, 1, debug = true) == 315)
  }

  test("step1 on examples") {
    assert(step1(example1) == 32000000)
    assert(step1(example2) == 11687500)
  }

  test("step1 on input") {
    assert(step1(input) == 925955316)
  }

  test("step2 on input") {
    assert(step2(input) == 241528477694627L)
  }
}
