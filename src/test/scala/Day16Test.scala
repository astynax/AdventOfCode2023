package me.astynax

import Day16._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day16Test extends AnyFunSuiteLike {
  private lazy val example = e("""
       .|...\....
       |.-.\.....
       .....|-...
       ........|.
       ..........
       .........\
       ..../.\\..
       .-.-/..|..
       .|....-|.\
       ..//.|....
       """.stripIndent().tail)

  private def e(s: String) = decode {
    s.lines().toScala(List)
  }

  test("decoding") {
    assert(example.map.nonEmpty)
    assert(input.map.nonEmpty)
  }

  test("step1 on example") {
    assert(step1(example) == 46)
  }

  test("step1 on input") {
    assert(step1(input) == 6795)
  }

  test("step2 on example") {
    assert(step2(example) == 51)
  }

  test("step2 on input") {
    assert(step2(input) == 7154)
  }
}
