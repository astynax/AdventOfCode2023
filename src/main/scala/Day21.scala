package me.astynax

import scala.annotation.tailrec

object Day21 {
  case class Input(start: Pos, blocks: Set[Pos], size: Int)

  @tailrec
  def fill(blocks: Set[Pos],
           size: Int,
           plan: Set[Pos],
           step: Int): Set[Pos] =
    if (step == 0)
      plan
    else {
      fill(
        blocks = blocks,
        size = size,
        plan = plan.flatMap(_.neighbours4)
          .filter { p => !(blocks contains p.bounded(size, size)) },
        step = step - 1,
      )
    }

  def step1(input: Input): Int = countStopsAt(input, 64)

  def step2(input: Input): Long = {
    val n = (26501365L - input.start.x) / input.size

    val x0 = 3847L // countStopsAt(input, 65)
    val x1 = 34165L // countStopsAt(input, 65 + 131)
    val x2 = 94697L // countStopsAt(input, 65 + 131 * 2)

    val b = x1 - x0
    val c = x2 - x1
    x0 + b * n + (n * (n - 1) / 2) * (c - b)
  }

  def countStopsAt(input: Input, steps: Int): Int = {
    fill(input.blocks, input.size, Set(input.start), steps).size
  }

  def decode(lines: List[String]): Input = {
    val blocks = Map2d.fromLines(lines)
    val start = blocks.find { case (_, c) => c == 'S' }.get._1
    Input(start, blocks.keys.toSet - start, lines.size
    )
  }

  lazy val input: Input = decode(me.astynax.Input.linesFrom("Day21.input"))
}
