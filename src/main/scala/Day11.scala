package me.astynax

import scala.annotation.tailrec
import scala.math.abs

object Day11 {

  case class BPos(x: Long, y: Long) {
    def distanceTo(other: BPos): Long = {
      abs(x - other.x) + abs(y - other.y)
    }

    def move(dx: Long = 0, dy: Long = 0): BPos = BPos(x + dx, y + dy)
  }

  def step1(input: Input): Long = calculate(input, 2)

  def step2(input: Input): Long = calculate(input, 1000000)

  def calculate(input: Input, coefficient: Long): Long = {
    input.expand(coefficient - 1).galaxies.toSeq
      .combinations(2)
      .map {
        case List(a, b) =>
          a distanceTo b
        case _ => Oops("Impossible!")
      }.sum

  }

  case class Input(galaxies: Set[BPos],
                   xs: Set[Long],
                   ys: Set[Long]) {
    def expand(coefficient: Long = 1): Input = {
      val gs = Input.heighten(
        ys.min,
        Input.widen(
          xs.min,
          galaxies,
          coefficient,
        ),
        coefficient,
      )
      Input(gs, gs.map(_.x), gs.map(_.y))
    }
  }

  object Input {
    @tailrec
    def widen(x: Long,
              s: Set[BPos],
              coefficient: Long = 1): Set[BPos] = {
      if (s.exists(_.x == x))
        widen(x + 1, s, coefficient)
      else if (s.exists(_.x > x))
        widen(
          x + 1 + coefficient,
          s.map { p =>
            if (p.x > x) p.move(dx = coefficient)
            else p
          },
          coefficient
        )
      else s
    }

    @tailrec
    def heighten(y: Long,
                 s: Set[BPos],
                 coefficient: Long = 1): Set[BPos] = {
      if (s.exists(_.y == y))
        heighten(y + 1, s, coefficient)
      else if (s.exists(_.y > y))
        heighten(
          y + 1 + coefficient,
          s.map { p =>
            if (p.y > y) p.move(dy = coefficient)
            else p
          },
          coefficient,
        )
      else s
    }
  }

  def decode(rows: List[String]): Input = {
    val galaxies = (for {
      (row, y) <- rows.zipWithIndex
      (c, x) <- row.zipWithIndex
      if c == '#'
    } yield BPos(x, y)).toSet
    val xs = galaxies.map(_.x)
    val ys = galaxies.map(_.y)
    Input(galaxies, xs, ys)
  }

  lazy val input: Input = decode(me.astynax.Input.linesFrom("Day11.input"))
}
