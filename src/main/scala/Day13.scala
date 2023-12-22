package me.astynax

object Day13 {
  type Input = List[Pattern]

  case class Pattern(cells: List[List[Char]]) {
    def mh(smudges: Int): Option[Int] = findMirror(cells, smudges)

    def mv(smudges: Int): Option[Int] = findMirror(Lists.transpose(cells), smudges)
  }

  private def findMirror(xs: Iterable[List[Char]], smudges: Int): Option[Int] = {
    val l = xs.head.length
    val as = xs.map(_.toArray)
    (1 until l).find { i =>
      val p = i min (l - i)
      val errors = as.map { arr =>
        (0 until p).count(j => arr(i - 1 - j) != arr(i + j))
      }.sum
      errors == smudges
    }
  }

  def step1: Input => Int = solveWith(smudges = 0)

  def step2: Input => Int = solveWith(smudges = 1)

  private def solveWith(smudges: Int)(input: Input): Int = input
    .map { p =>
      p.mv(smudges).map(_ * 100).orElse(p.mh(smudges)).get
    }.sum

  def decode(lines: List[String]): Input =
    Lists.splitBy(lines)(_.isEmpty)
      .map { p => Pattern(p.map(_.toList)) }
      .toList

  lazy val input: Input = decode(Input.linesFrom("Day13.input"))
}
