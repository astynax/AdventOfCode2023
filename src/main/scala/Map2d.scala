package me.astynax

object Map2d {
  def fromLines(lines: List[String],
                good: Char => Boolean = _ != '.'): Map[Pos, Char] = (for {
    (row, y) <- lines.zipWithIndex
    (c, x) <- row.zipWithIndex
    if good(c)
  } yield Pos(x, y) -> c).toMap

  def dump[A](m: Map[Pos, A], toChar: A => Char): Unit = {
    val w = m.map { case (Pos(x, _), _) => x }.max
    val h = m.map { case (Pos(_, y), _) => y }.max
    (0 to h).foreach { y =>
      println((0 to w).map { x =>
        toChar(m(Pos(x, y)))
      }.mkString)
    }
  }
  
  def dump(m: Map[Pos, Char]): Unit = dump[Char](m, identity)
}
