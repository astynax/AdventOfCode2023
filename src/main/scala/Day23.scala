package me.astynax

import scala.annotation.tailrec

object Day23 {
  case class Input(start: Pos, stop: Pos, map: Map[Pos, Char])

  def step1(input: Input): Int = walk(input, forkAndSlip(input)).max

  // FIXME: make a weighted graph!
  def step2(input: Input): Int = walk(input, justFork).max

  private def walk(input: Input, fork: Pos => List[Pos]): Set[Int] = {
    @tailrec
    def step(ps: List[(Pos, Int, Set[Pos])], paths: Set[Int], cache: Map[Pos, Int]): Set[Int] = {
      if (ps.isEmpty) paths
      else {
        val newPs = for {
          (p, d, vs) <- ps
          dd = d + 1
          vvs = vs + p
          x <- fork(p)
          if (input.map contains x) & !(vvs contains x) & cache.getOrElse(x, 0) <= d
        } yield (x, dd, vvs)
        step(
          ps = newPs.filter(_._1 != input.stop).sortBy(-_._2).take(10000),
          paths = paths ++ (for {
            (p, d, _) <- newPs
            if p == input.stop
          } yield d),
          cache = cache ++ newPs.map { case (p, d, _) => p -> d },
        )
      }
    }

    step(List((input.start, 0, Set.empty)), Set.empty, Map.empty)
  }

  private def forkAndSlip(input: Input)(p: Pos) = {
    input.map(p) match {
      case '>' => List(p.move(dx = 1))
      case '<' => List(p.move(dx = -1))
      case 'v' => List(p.move(dy = 1))
      case '^' => List(p.move(dy = -1))
      case _ => p.neighbours4
    }
  }

  private def justFork: Pos => List[Pos] = _.neighbours4

  def decode(lines: List[String]): Input = {
    val lastRow = lines.size - 1
    val map = Map2d.fromLines(lines, _ != '#')
    val positions = map.keys
    Input(
      start = positions.find(_.y == 0).get,
      stop = positions.find(_.y == lastRow).get,
      map = map
    )
  }

  lazy val input: Input = decode(me.astynax.Input.linesFrom("Day23.input"))
}
