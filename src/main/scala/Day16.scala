package me.astynax

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.postfixOps

object Day16 {
  case class Stage(map: Map[Pos, Char], width: Int, height: Int) {
    def contains(pos: Pos): Boolean =
      pos.x >= 0 & pos.x < width & pos.y >= 0 & pos.y < height
  }

  def step1(stage: Stage): Int = Tracer(stage, Pos(-1, 0) -> '>')

  def step2(stage: Stage): Int = (
    (0 until stage.width).flatMap { x => List(
      Pos(x, -1) -> 'v',
      Pos(x, stage.height) -> '^',
    )} ++ (0 until stage.height).flatMap { y => List(
      Pos(-1, y) -> '>',
      Pos(stage.width, y) -> '<',
    )})
    .map { Tracer(stage, _) }
    .max

  private object Tracer {
    private val `\\` = Map(
      '^' -> '<',
      'v' -> '>',
      '<' -> '^',
      '>' -> 'v',
    )

    private val `/` = Map(
      '^' -> '>',
      'v' -> '<',
      '<' -> 'v',
      '>' -> '^',
    )

    def apply(stage: Stage, start: (Pos, Char)): Int = {
      val energizedSpaces = mutable.Map.empty[Pos, Set[Char]]

      @tailrec
      def go(beams: List[(Pos, Char)]): Unit =
        if (beams.nonEmpty) {
          val bs = beams.flatMap { case (p, dir) =>
            val next = nextPos(p, dir)
            val ns = stage.map.get(next).map { dev: Char =>
              dev match {
                case '|' => splitOrPass(next, dir, '^', 'v')
                case '-' => splitOrPass(next, dir, '<', '>')
                case '/' => List(next -> `/`(dir))
                case '\\' => List(next -> `\\`(dir))
              }
            }.getOrElse {
              List(next -> dir)
            }
            ns
          }.filter { case (p, d) =>
            stage.contains(p) & !energizedSpaces.getOrElse(p, Set.empty).contains(d)
          }
          bs.foreach { case (p, d) =>
            energizedSpaces.updateWith(p) { s =>
              s.map(_ + d).orElse(Some(Set(d)))
            }
          }
          go(bs)
        }

      go(List(start))

      energizedSpaces.size
    }

    private def nextPos(p: Pos, dir: Char): Pos = dir match {
      case '<' => p.move(dx = -1)
      case '>' => p.move(dx = 1)
      case '^' => p.move(dy = -1)
      case _ => p.move(dy = 1)
    }

    private def splitOrPass(pos: Pos, dir: Char, d1: Char, d2: Char) = {
      if (dir == d1 | dir == d2) List(pos -> dir)
      else List(pos -> d1, pos -> d2)
    }
  }

  def decode(lines: List[String]): Stage = {
    val width = lines.head.length
    val height = lines.length
    val map = Map2d.fromLines(lines)
    Stage(map, width, height)
  }

  lazy val input: Stage = decode(me.astynax.Input.linesFrom("Day16.input"))
}
