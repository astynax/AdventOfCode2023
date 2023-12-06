package me.astynax

object Day06 {
  type Input = List[(BigInt, BigInt)]

  def probe(time: BigInt, record: BigInt): BigInt =
    (BigInt(1) until time).map { t =>
      (time - t) * t
    }.count(_ > record)

  def conjoin(input: Input): (BigInt, BigInt) = {
    val (a, b) = input.unzip
    BigInt(a.mkString) -> BigInt(b.mkString)
  }

  def step1(races: Input): BigInt =
    races.map { case (a, b) => probe(a, b) }.product

  def step2(races: Input): BigInt = {
    val (a, b) = conjoin(races)
    probe(a, b)
  }

  lazy val input: Input = {
    val List(l1, l2) = Input.linesFrom("Day06.input").take(2).map {
      raw"\s+".r.split(_).drop(1).map(BigInt(_))
    }
    (l1 zip l2).toList
  }
}
