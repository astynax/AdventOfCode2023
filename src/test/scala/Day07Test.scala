package me.astynax

import Day07._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala
import scala.math.Ordering.Implicits.seqOrdering

class Day07Test extends AnyFunSuiteLike {
  test("decoding") {
    assert(decode("7AAA7 384") == Hand(List('7', 'A', 'A', 'A', '7'), 384))
  }

  private val example: Input =
    """|32T3K 765
       |T55J5 684
       |KK677 28
       |KTJJT 220
       |QQQJA 483
       |""".stripMargin.lines().toScala(List).map(decode)

  private def h(s: String): Hand = Hand(s.toList, -1)

  test("decoding of example") {
    assert(example.nonEmpty)
  }

  test("decoding of input") {
    assert(input.nonEmpty)
  }

  test("scoring") {
    assert(example.map(_.score) == List(1, 3, 2, 2, 3))
    assert(h("AK234").score == 0)
    assert(h("AA234").score == 1)
    assert(h("AKKAK").score == 4)
    assert(h("AAAAK").score == 5)
    assert(h("AAAAA").score == 6)
    assert(input.map(_.score).nonEmpty)
  }

  test("comparison") {
    assert(List(h("33332"), h("2AAAA")).sortBy(System1.apply) == List(
      h("2AAAA"), h("33332")
    ))
    assert(example.sortBy(System1.apply).map(_.cards.mkString) == List(
      "32T3K", "KTJJT", "KK677", "T55J5", "QQQJA"
    ))
  }

  test("step1 on example") {
    assert(step1(example) == 6440)
  }

  test("step1 on input") {
    assert(step1(input) == 248559379)
  }

  test("bestScore") {
    assert(System2.bestScore(h("QJJQ2")) == 5)
    assert(System2.bestScore(h("QQQJA")) == 5)
  }

  test("step2 on example") {
    assert(step2(example) == 5905)
  }

  test("step2 on input") {
    assert(step2(input) == 249631254)
  }

}
