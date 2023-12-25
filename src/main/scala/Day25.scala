package me.astynax

object Day25 {
  type Input = Map[String, Set[String]]

  // FIXME: try to do some graph clustering by myself
  // But for now I just used Python+networkx :(

  def decode(lines: List[String]): Input = lines
    .flatMap { line =>
      val n :: ns = line.split(raw":?\s+").toList
      ns.map(_ -> n) ++ ns.map(n -> _)
    }.foldLeft(Map.empty[String, Set[String]]) { (acc, link) =>
      val (f, t) = link
      acc.updated(f, acc.getOrElse(f, Set.empty) + t)
    }

  lazy val input: Input = decode(Input.linesFrom("Day25.input"))
}
