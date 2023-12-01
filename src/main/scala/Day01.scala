package me.astynax

object Day01 {
  type Input = List[String]

  private val digitValue: Map[String, Int] =
    List(
      "zero", "one", "two", "three", "four", "five",
      "six", "seven", "eight", "nine"
    ).zipWithIndex
      .flatMap { case (s, i) =>
        List(s -> i, i.toString -> i)
      }.toMap

  private val digits: List[String] = digitValue.keys.toList

  def findAll(line: String): List[String] =
    line.tails.flatMap { t =>
      digits.find(t.startsWith).toList
    }.toList

  def step1(lines: Input): Int =
    lines.map { line =>
      val digits = line.filter(_.isDigit).toList
      digits.head.asDigit * 10 + digits.last.asDigit
    }.sum

  def step2(lines: Input): Int =
    lines.map { line =>
      val digits = findAll(line)
      digitValue(digits.head) * 10 + digitValue(digits.last)
    }.sum

  lazy val input: Input = Input.linesFrom("Day01.input")
}
