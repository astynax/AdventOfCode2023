package me.astynax

import Day25._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day25Test extends AnyFunSuiteLike {
  private lazy val example = decode(
    """jqt: rhn xhk nvd
      |rsh: frs pzl lsr
      |xhk: hfx
      |cmg: qnr nvd lhk bvb
      |rhn: xhk bvb hfx
      |bvb: xhk hfx
      |pzl: lsr hfx nvd
      |qnr: nvd
      |ntq: jqt hfx bvb xhk
      |nvd: lhk
      |lsr: lhk
      |rzs: qnr cmg lsr rsh
      |frs: qnr lhk lsr
      |""".stripMargin.lines().toScala(List)
  )

  test("decoding") {
    assert(decode(List(
      "a: b c",
      "c: d",
    )) == Map(
      "a" -> Set("b", "c"),
      "b" -> Set("a"),
      "c" -> Set("a", "d"),
      "d" -> Set("c"),
    ))
    assert(example.nonEmpty)
    assert(input.nonEmpty)
  }
}
