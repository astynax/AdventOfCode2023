package me.astynax

import scala.math.Ordering.Implicits.seqOrdering

object Day07 {
  type Input = List[Hand]

  case class Hand(cards: List[Char], bid: Int) {
    lazy val score: Int = Hand.types(
      cards.groupBy(identity).values.toList.map(_.size).sorted
    )

    lazy val ranks: List[Int] = cards.map(Hand.ranks)
  }

  object Hand {
    private val types: Map[List[Int], Int] = List(
      List(5),
      List(1, 4),
      List(2, 3),
      List(1, 1, 3),
      List(1, 2, 2),
      List(1, 1, 1, 2),
      List(1, 1, 1, 1, 1),
    ).reverse.zipWithIndex.toMap

    val ranks: Map[Char, Int] = "AKQJT98765432".reverse.zipWithIndex.toMap
  }

  trait ScoringSystem {
    type Key = (Int, List[Int])
    def apply(x: Hand): Key
  }

  object System1 extends ScoringSystem {
    def apply(x: Hand): Key = (x.score, x.ranks)
  }

  object System2 extends ScoringSystem {

    private val j = Hand.ranks('J')

    def bestScore(h: Hand): Int = {
      val notJs = h.cards.filterNot { _ == 'J' }
      if (notJs.isEmpty) h.score
      else {
        val cs = h.cards.toArray

        def iter(card: Char) = if (card == 'J') notJs else List(card)

        (for {
          a <- iter(cs(0))
          b <- iter(cs(1))
          c <- iter(cs(2))
          d <- iter(cs(3))
          e <- iter(cs(4))
        } yield Hand(List(a, b, c, d, e), 0).score
          ).max
      }
    }

    override def apply(x: Hand): Key = (
      bestScore(x),
      x.ranks.map { x => if (x == j) -1 else x }
    )
  }

  def step1(hands: Input): Int = total(hands, System1)

  def step2(hands: Input): Int = total(hands, System2)

  private def total(hands: Input, system: ScoringSystem): Int =
    hands.sortBy(system.apply).zipWithIndex
      .map { case (h, i) => h.bid * (i + 1) }.sum

  def decode(line: String): Hand = Hand(line.take(5).toList, line.drop(6).toInt)

  lazy val input: Input = Input.linesFrom("Day07.input").map(decode)
}
