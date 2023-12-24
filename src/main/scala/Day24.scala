package me.astynax

object Day24 {
  type Input = List[Hailstone]

  case class Hailstone(x: BigInt, y: BigInt, z: BigInt, vx: Int, vy: Int, vz: Int)

  // I just gave up and used a Python + Z3 prover

  def decode(lines: List[String]): Input = lines.map { line =>
    val parts = line.split(raw"\s+").map(_.stripSuffix(","))
    Hailstone(
      x = BigInt(parts(0)),
      y = BigInt(parts(1)),
      z = BigInt(parts(2)),
      vx = parts(4).toInt,
      vy = parts(5).toInt,
      vz = parts(6).toInt,
    )
  }

  lazy val input: Input = decode(Input.linesFrom("Day24.input"))
}
