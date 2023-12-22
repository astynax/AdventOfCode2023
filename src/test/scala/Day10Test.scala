package me.astynax

import Day10._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day10Test extends AnyFunSuiteLike {
  private val example = decode(
    """7-F7-
      |.FJ|7
      |SJLL7
      ||F--J
      |LJ.LJ
      |""".stripMargin.lines().toScala(List)
  )

  private lazy val though = decode("""
       FF7FSF7F7F7F7F7F---7
       L|LJ||||||||||||F--J
       FL-7LJLJ||||||LJL-77
       F--JF--7||LJLJ7F7FJ-
       L---JF-JLJ.||-FJLJJ7
       |F|F-JF---7F7-L7L|7|
       |FFJF7L7F-JF7|JL---7
       7-L-JL7||F7|L7F-7F7|
       L.L7LFJ|||||FJL7||LJ
       L7JLJL-JLJLJL--JLJ.L
       """.stripIndent.lines().toScala(List).drop(1)
  )

  test("example decoding") {
    assert(example.pipes.nonEmpty)
  }

  test("input decoding") {
    assert(input.pipes.nonEmpty)
  }

  test("debug printing") {
    Map2d.dump(example.pipes)
  }

  test("loop searching") {
    assert(findLoop(example) contains Pos(4, 2))
  }

  test("loop searching in a though case") {
    assert(findLoop(though).last == Pos(3, 0))
  }

  test("step1 on example") {
    assert(step1(example) == 8)
  }

  test("step1 on input") {
    assert(step1(input) == 6890)
  }

  test("rules") {
    assert(rules(((0, 1), (-1, 0))).size == 5)
  }

  test("step2 on examples") {
    val e = decode(
      """..........
        |.S------7.
        |.|F----7|.
        |.||....||.
        |.||....||.
        |.|L-7F-J|.
        |.|..||..|.
        |.L--JL--J.
        |..........
        |""".stripMargin.lines().toScala(List)
    )
    assert(step2(e) == 4)

    val e2 = decode(
      """.F----7F7F7F7F-7....
        |.|F--7||||||||FJ....
        |.||.FJ||||||||L7....
        |FJL7L7LJLJ||LJ.L-7..
        |L--J.L7...LJS7F-7L7.
        |....F-J..F7FJ|L7L7L7
        |....L7.F7||L7|.L7L7|
        |.....|FJLJ|FJ|F7|.LJ
        |....FJL-7.||.||||...
        |....L---J.LJ.LJLJ...
        |""".stripMargin.lines().toScala(List)
    )
    assert(step2(e2) == 8)

    val e3 = decode(
      """........
        |.S----7.
        |.|....|.
        |.|.|..|.
        |.|....|.
        |.L----J.
        |""".stripMargin.stripMargin.lines().toScala(List)
    )

    assert(step2(e3) == 12)
  }

  test("step2 on a though example") {
    assert(step2(though) == 10)
  }

  test("step2 on input") {
    assert(step2(input) == 453)
  }
}
