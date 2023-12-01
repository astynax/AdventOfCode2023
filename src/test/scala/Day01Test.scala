package me.astynax

import scala.jdk.StreamConverters._

import org.scalatest.funsuite.AnyFunSuiteLike

import Day01._

class Day01Test extends AnyFunSuiteLike {

  test("step1 on example") {
    assert(step1(
      """1abc2
        |pqr3stu8vwx
        |a1b2c3d4e5f
        |treb7uchet
        |""".stripMargin.lines().toScala(List)
    ) == 142)
  }

  test("step1 on input") {
    assert(step1(input) == 53921)
  }

  test("pattern in a complex case") {
    assert(findAll("8eightlvc8ndtnf5eightwon") == List(
      "8", "eight", "8", "5", "eight", "two"
    ))
  }

  test("step2 on example") {
    assert(step2(
      """two1nine
        |eightwothree
        |abcone2threexyz
        |xtwone3four
        |4nineeightseven2
        |zoneight234
        |7pqrstsixteen
        |""".stripMargin.lines().toScala(List)
    ) == 281)
  }

  test("step2 on input") {
    assert(step2(input) == 54676)
  }
}
