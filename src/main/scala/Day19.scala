package me.astynax

import scala.annotation.tailrec
import scala.language.existentials

object Day19 {
  case class Input(workflows: Map[Name, Workflow], parts: List[Part])

  case class Name(name: String)

  case class Workflow(rules: List[Rule], wildcard: Action) {
    def apply(part: Part): Action = Lists
      .firstSome(rules)(_.apply(part)).getOrElse(wildcard)
  }

  object Category extends Enumeration {
    type Category = Value
    val X, M, A, S = Value
  }

  import Category._

  object LtGt extends Enumeration {
    type LtGt = Value
    val Lt, Gt = Value
  }

  import LtGt._

  case class Rule(category: Category, comparison: LtGt, threshold: Int, action: Action) {
    def apply(part: Part): Option[Action] = {
      val expected = comparison match {
        case Lt => -1
        case Gt => 1
      }
      if (part(category).compare(threshold) == expected)
        Some(action)
      else None
    }
  }

  case class Part(rates: Map[Category, Int]) {
    def apply(category: Category): Int = rates(category)

    def sum: Int = rates.values.sum
  }

  sealed trait Action

  object Action {
    case class GoTo(name: Name) extends Action

    case object Reject extends Action

    case object Accept extends Action
  }

  def step1(input: Input): Int = input
    .parts.filter(isAccepted(input, _)).map(_.sum).sum

  def isAccepted(input: Input, part: Part): Boolean = {
    @tailrec
    def walk(name: Name): Boolean = input.workflows(name)(part) match {
      case Action.Accept => true
      case Action.Reject => false
      case Action.GoTo(n) => walk(n)
    }
    walk(Name("in"))
  }

  private case class PartRange(ranges: Map[Category, (Int, Int)]) {
    def apply(rule: Rule): (PartRange, PartRange) = {
      val (f, t) = ranges(rule.category)
      val (l, r) = if (rule.comparison == Lt)
        (f, rule.threshold - 1) -> (rule.threshold, t)
      else
        (rule.threshold + 1, t) -> (f, rule.threshold)
      (
        PartRange(ranges.updated(rule.category, l)),
        PartRange(ranges.updated(rule.category, r)),
      )
    }

    def power: BigInt = ranges.values
      .map { case (f, t) => BigInt(t - f + 1) }.product
  }

  private object PartRange {
    def whole: PartRange = PartRange(List(X, M, A, S).map(_ -> (1, 4000)).toMap)
  }


  def step2(input: Input): BigInt = {

    def collect(n: Name, pr: PartRange): List[PartRange] = {
      val wf = input.workflows(n)
      val (r, results) = wf.rules.foldLeft(pr -> List[PartRange]()) { (acc, rule) =>
        val (x, xs) = acc
        val (l, r) = x(rule)
        val ys = act(l, rule.action)
        (r, xs ++ ys)
      }
      results ++ act(r, wf.wildcard)
    }

    def act(pr: PartRange, action: Action): List[PartRange] = action match {
      case Action.Accept => List(pr)
      case Action.Reject => List()
      case Action.GoTo(n) => collect(n, pr)
    }

    val ranges = collect(Name("in"), PartRange.whole)

    ranges.map(_.power).sum
  }

  private def decodeRule(s: String): Rule = {
    val List(cond, action) = s.split(':').toList
    Rule(
      category = categoryFromChar(cond.head),
      comparison = cond(1) match {
        case '<' => Lt
        case '>' => Gt
        case _ => Oops(s"Bad comparison in rule $s")
      },
      threshold = cond.drop(2).toInt,
      action = decodeAction(action)
    )
  }

  private def decodeAction: String => Action = {
    case "R" => Action.Reject
    case "A" => Action.Accept
    case name => Action.GoTo(Name(name))
  }

  private def decodeWorkflow(line: String): (Name, Workflow) = {
    val List(name, rest) = line.split('{').toList
    val chunks = rest.dropRight(1).split(',').toList
    Name(name) -> Workflow(chunks.init.map(decodeRule), decodeAction(chunks.last))
  }

  private def decodePart(line: String): Part = {
    val attrs = line.drop(1).dropRight(1).split(',')
    Part(attrs.map { s => (
      categoryFromChar(s.head),
      s.drop(2).toInt
    )}.toMap)
  }

  private def categoryFromChar: Char => Category = {
    case 'x' => X
    case 'm' => M
    case 'a' => A
    case 's' => S
    case x => Oops(s"Bad category: $x")
  }

  def decode(lines: List[String]): Input = {
    val List(rawWs, rawPs) = Lists.splitBy(lines)(_.isEmpty)

    val ws = rawWs.map(decodeWorkflow)
    val ps = rawPs.map(decodePart)

    Input(ws.toMap, ps)
  }

  lazy val input: Input = decode(me.astynax.Input.linesFrom("Day19.input"))
}
