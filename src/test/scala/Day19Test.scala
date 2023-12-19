package me.astynax

import Day19._

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

class Day19Test extends AnyFunSuiteLike {
  private lazy val example = decode(
    """px{a<2006:qkq,m>2090:A,rfg}
      |pv{a>1716:R,A}
      |lnx{m>1548:A,A}
      |rfg{s<537:gd,x>2440:R,A}
      |qs{s>3448:A,lnx}
      |qkq{x<1416:A,crn}
      |crn{x>2662:A,R}
      |in{s<1351:px,qqz}
      |qqz{s>2770:qs,m<1801:hdj,R}
      |gd{a>3333:R,R}
      |hdj{m>838:A,pv}
      |
      |{x=787,m=2655,a=1222,s=2876}
      |{x=1679,m=44,a=2067,s=496}
      |{x=2036,m=264,a=79,s=2244}
      |{x=2461,m=1339,a=466,s=291}
      |{x=2127,m=1623,a=2188,s=1013}
      |""".stripMargin.lines().toScala(List)
  )

  test("decoding") {
    assert(example.workflows.nonEmpty)
    assert(example.parts.nonEmpty)
    assert(example.workflows(Name("px")) == Workflow(
      List(
        Rule(Category.A, LtGt.Lt, 2006, Action.GoTo(Name("qkq"))),
        Rule(Category.M, LtGt.Gt, 2090, Action.Accept),
      ),
      Action.GoTo(Name("rfg"))
    ))
    assert(input.workflows.nonEmpty)
  }

  test("rule applying") {
    val r1 = Rule(Category.X, LtGt.Lt, 1000, Action.Accept)
    assert(r1(Part(Map(Category.X -> 999))) contains Action.Accept)
    assert(r1(Part(Map(Category.X -> 1001))).isEmpty)
    val r2 = Rule(Category.M, LtGt.Gt, 0, Action.Reject)
    assert(r2(Part(Map(Category.M -> 1))) contains Action.Reject)
    assert(r2(Part(Map(Category.M -> -1))).isEmpty)
  }

  test("acceptance") {
    assert(example.parts.map(isAccepted(example, _)) == List(
      true, false, true, false, true,
    ))
  }

  test("step1 on example") {
    assert(step1(example) == 19114)
  }

  test("step1 on input") {
    assert(step1(input) == 332145)
  }

  test("step2 on example") {
    assert(step2(example) == 167409079868000L)
  }

  test("step2 on input") {
    assert(step2(input) == 136661579897555L)
  }
}
