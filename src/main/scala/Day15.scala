package me.astynax

object Day15 {
  type Input = List[Step]
  type Step = String
  type Box = List[(String, Int)]

  def step1(input: Input): Int = input.map(hashIt).sum

  def step2(input: Input): Int = interpret(input)
    .toSeq.map { case (box, lenses) =>
      lenses.zipWithIndex
        .map { case ((_, fl), i) => (box + 1) * (i + 1) * fl }
        .sum
    }.sum

  def interpret(input: Input): Map[Int, Box] = input
    .foldLeft(Map.empty[Int, Box]) { (acc, step) =>
      val label = step.takeWhile { c => c != '=' & c != '-' }
      val hash = hashIt(label)
      if (step.endsWith("-")) {
        acc.updatedWith(hash)(_.map(_.filter(_._1 != label)))
      } else {
        val fl = step.split('=').last.toInt
        acc.updatedWith(hash) {
          case None => Some(List(label -> fl))
          case Some(xs) => Some(
            if (xs.exists(_._1 == label))
              xs.map { case (k, v) => k -> (if (k == label) fl else v) }
            else xs appended (label -> fl)
          )
        }
      }
    }

  def hashIt(s: String): Int = s.foldLeft(0) { (acc, c) =>
    ((acc + c.toInt) * 17) % 256
  }

  lazy val input: Input = decode(Input.linesFrom("Day15.input").head)

  def decode(line: Step): List[Step] = line.split(',').toList
}
