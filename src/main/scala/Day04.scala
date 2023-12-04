package me.astynax

object Day04 {
  type Input = List[Card]

  case class Card(id: Int, winning: Set[Int], numbers: Set[Int]) {
    val matches: Int = winning.intersect(numbers).size
  }

  object Card {
    def fromString(s: String): Card = {
      val rawId :: words = space.split(s).drop(1).toList
      val sep = words.indexOf("|")
      val (ws, ns) = words.splitAt(sep)
      Card(
        id = rawId.dropRight(1).toInt,
        winning = ws.map(_.toInt).toSet,
        numbers = ns.tail.map(_.toInt).toSet,
      )
    }

    private val space = raw"\s+".r
  }

  def step1(cards: Input): Int =
    cards.map { c =>
      if (c.matches > 0) scala.math.pow(2, c.matches - 1).toInt else 0
    }.sum

  def step2(cards: Input): Int =
    walk(
      cards.map { c => c.id -> c.matches }.toMap,
      cards.map{ c => c.id -> 1 }.toMap,
      cards.map { _.id },
    ).values.sum

  private def walk(matches: Map[Int, Int],
                   counts: Map[Int, Int],
                   ids: List[Int]) =
    ids.tails.toList.dropRight(1)
      .foldLeft(counts) { (cs, xs) =>
        val x :: rest = xs
        val m = matches(x)
        val n = cs(x)
        rest.take(m).foldLeft(cs) { (v, k) => v.updatedWith(k) {
          _.map(_ + n)
        } }
      }

  lazy val input: Input = Input.linesFrom("Day04.input").map(Card.fromString)
}
