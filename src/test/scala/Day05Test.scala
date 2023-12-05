package me.astynax

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

import Day05._

class Day05Test extends AnyFunSuiteLike {
  test("decoding") {
    assert(decode(List(
      "seeds: 10 20",
      "",
      "a-to-b map:",
      "10 20 5",
      "30 45 2",
      "",
      "b-to-c map:",
      "30 50 10"
    )) == (List(10, 20), List(
      Mapping("a-to-b", List(
        (10, 20, 5),
        (30, 45, 2),
      )),
      Mapping("b-to-c", List(
        (30, 50, 10),
      ))
    )))
  }

  lazy val example: Input = decode(
    """|seeds: 79 14 55 13
       |
       |seed-to-soil map:
       |50 98 2
       |52 50 48
       |
       |soil-to-fertilizer map:
       |0 15 37
       |37 52 2
       |39 0 15
       |
       |fertilizer-to-water map:
       |49 53 8
       |0 11 42
       |42 0 7
       |57 7 4
       |
       |water-to-light map:
       |88 18 7
       |18 25 70
       |
       |light-to-temperature map:
       |45 77 23
       |81 45 19
       |68 64 13
       |
       |temperature-to-humidity map:
       |0 69 1
       |1 0 69
       |
       |humidity-to-location map:
       |60 56 37
       |56 93 4
       |""".stripMargin.lines().toScala(List)
  )

  test("example decoding") {
    assert(example._2.size == 7)
  }

  test("mapping") {
    val m = Mapping("", List(
      (20, 10, 3),
      (0, 50, 5),
    ))
    assert(List(
      7, 11, 14, 49, 55, 60
    ).map(m(_)) == List(
      7, 21, 14, 49, 5, 60
    ))
  }

  test("step1 on example") {
    assert(step1(example) == 35)
  }

  test("step1 on input") {
    assert(step1(input) == 318728750)
  }

  test("mapping with ranges") {
    val m = Mapping("", List(
      (100, 5, 3),
      (50, 5, 10),
      (50, 0, 10),
    ))
    assert(m.process((1, 10)) == List())
  }

  test("step2 on example") {
    assert(step2(example) == 46)
  }

  test("step2 on input") {
    assert(step2(input) == 37384986)
  }
}
