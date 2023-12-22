package me.astynax

object Day05 {
  type Rng = (BigInt, BigInt)
  type Input = (List[BigInt], List[Mapping])

  case class Mapping(name: String, rules: List[(BigInt, BigInt, BigInt)]) {
    def apply(value: BigInt): BigInt =
      rules.find { case (_, f, r) => value >= f & value <= (f + r) }
        .map { case (t, f, _) => value - f + t }
        .getOrElse(value)

    def process(value: Rng): List[Rng] = {
      val (rs, done) = rules.foldLeft(
        (List[Rng](value), List.empty[Rng])
      ) { case ((rs, done), (t, f, w)) =>
        val rule = (f, f + w - 1)
        val off = t - f
        rs.foldLeft(
          (List.empty[Rng], done)
        ) { case ((newRs, newDone), r) =>
          Ranges.associate[BigInt](rule, r) match {
            case Ranges.Disjointed() =>
              (newRs appended r, newDone)

            case Ranges.Includes(_, i, _) =>
              (newRs, newDone appended Ranges.offset(i, off))

            case Ranges.Included(l, i, r) => (
              consSome(l, consSome(r, newRs)),
              newDone appended Ranges.offset(i, off)
            )

            case Ranges.Overlaps(l, i, r) => (
              consIf(
                l._1 != rule._1, l,
                consIf(r._2 != rule._2, r,
                  newRs
                )
              ),
              newDone appended Ranges.offset(i, off)
            )
          }
        }
      }
      rs ++ done
    }
  }

  private def consIf[A](cond: Boolean, x: A, xs: List[A]) = if (cond) x :: xs else xs

  private def consSome[A](x: Option[A], xs: List[A]) = x.map(_ :: xs).getOrElse(xs)

  def step1(input: Input): BigInt = {
    val (seeds, maps) = input
    maps.foldLeft(seeds) { (s, m) => s.map(m.apply) }.min
  }

  def step2(input: Input): BigInt = {
    val (header, maps) = input
    val ranges = header.grouped(2).map {
      case List(a, b) => (a, a + b - 1)
      case _ => Oops("Impossible!")
    }
    maps.foldLeft(ranges) { (acc, m) =>
      acc.flatMap(m.process)
    }.map(_._1).min
  }

  def decode(lines: List[String]): Input = {
    val header :: chunks = Lists.splitBy(lines)(_.isEmpty)
    val seeds = header.head.stripPrefix("seeds: ").split(' ').toList.map(BigInt(_))
    val maps = chunks.map {
      case x :: xs =>
        Mapping(x.stripSuffix(" map:"), xs.map { line =>
          line.split(' ').toList.map(BigInt(_)) match {
            case List(t, f, r) => (t, f, r)
            case _ => Oops(s"Bad line: $line")
          }
        })
      case _ => Oops(s"Empty chunk!")
    }
    (seeds, maps)
  }

  lazy val input: Input = decode(Input.linesFrom("Day05.input"))
}
