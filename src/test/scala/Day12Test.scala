package me.astynax

import Day12._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day12Test extends AnyFunSuiteLike {
  private lazy val example: Input =
    """???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1
      |""".stripMargin.lines().toScala(List).map(decode)

  test("decoding") {
    assert(decode("... 42,55") == Item("...", List(42, 55)))
    assert(example.nonEmpty)
    assert(input.nonEmpty)
  }

  test("variant counting") {
    assert(decode("?###???????? 3,2,1").variantCount == 10)
  }

  test("step1 on example") {
    assert(step1(example) == 21)
  }

  test("step1 on input") {
    assert(step1(input) == 7307)
  }

  test("expanding") {
    assert(decode("# 1,2").expanded == decode(
      "#?#?#?#?# 1,2,1,2,1,2,1,2,1,2"
    ))
  }

  test("step2 on example") {
    assert(step2(example) == 525152)
  }

  test("step2 on input") {
    assert(step2(input) == 3415570893842L)
  }
}
