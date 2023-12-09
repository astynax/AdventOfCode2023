package me.astynax

object Day09 {
  type Input = List[History]
  private type History = List[BigInt]

  def step1: Input => BigInt = solveUsing(future, _.last)

  def step2: Input => BigInt = solveUsing(past, _.head)

  private def solveUsing(f: List[History] => History, select: History => BigInt)(values: Input) =
    values.map { h =>
      f(diffs(h))
    }.map(select).sum

  def future(diffs: List[History]): History =
    diffs.reverse.reduce { (h, r) =>
      r.appended(r.last + h.last)
    }

  def past(diffs: List[History]): History =
    diffs.reverse.reduce { (h, r) =>
      r.prepended(r.head - h.head)
    }

  def diffs(xs: History): List[History] =
    if (xs.forall(_ == 0)) List()
    else {
      val row: History =
        (xs zip xs.tail.appended(BigInt(0)))
          .map { case (a, b) => b - a }.dropRight(1)
      xs :: diffs(row)
    }

  def decode(line: String): History = line.split(' ').map(BigInt(_)).toList

  lazy val input: Input = Input.linesFrom("Day09.input").map(decode)
}
