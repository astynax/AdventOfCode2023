package me.astynax

import Pos._

object Day03 {
  type Input = Map[Pos, Char]

  def step1(schema: Input): Int =
    opsOf(schema).map(_._1)
      .flatMap(numbersNear(schema, _)).sum

  def step2(schema: Input): Int =
    opsOf(schema).filter { case (_, c) => c == '*' }.map(_._1)
      .map(numbersNear(schema, _))
      .filter(_.size == 2)
      .map(_.product)
      .sum

  private def numbersNear(schema: Input, p: Pos): Set[Int] =
    p.neighbours.flatMap(numberAt(schema, _).toList)
      .toSet  // it is a hack, but all neighbours for the op are unique

  private def opsOf(schema: Input): Seq[(Pos, Char)] =
    schema.toSeq.filter { case (_, v) => !v.isDigit }

  private def collectDigits(schema: Input,
                            pos: Pos, dx: Int,
                            acc: List[Char] = List()): List[Char] = {
    val newPos = pos.move(dx=dx)
    (for {
      d <- schema.get(newPos)
      if d.isDigit
    } yield collectDigits(schema, newPos, dx, d :: acc)
    ).getOrElse(acc.reverse)
  }

  def numberAt(schema: Input, pos: Pos): Option[Int] =
    for {
      x <- schema.get(pos)
      if x.isDigit
      l = collectDigits(schema, pos, -1).reverse
      r = collectDigits(schema, pos, 1)
    } yield ((l appended x) ++ r).mkString.toInt

  def decode(lines: List[String]): Input =
    lines.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (c, x) => Pos(x, y) -> c }
    }.filter { case (_, c) => c != '.' }.toMap

  lazy val input: Input = decode(Input.linesFrom("Day03.input"))
}
