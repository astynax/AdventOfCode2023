package me.astynax

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object Day12 {
  type Input = List[Item]

  case class Item(pattern: String, checksum: List[Int]) {
    def variantCount: Long = Day12.Solver(this)()

    def expanded: Item = Item(
      (for { _ <- 0 until 5 } yield pattern).mkString("?"),
      (for { _ <- 0 until 5 } yield checksum).flatten.toList
    )
  }

  private case class Solver(item: Item) {
    private type Key = (List[Char], List[Int])
    private type Cache = mutable.Map[Key, Long]

    private val goCache: Cache = mutable.Map.empty[Key, Long]
    private val advanceCache: Cache = mutable.Map.empty[Key, Long]

    def apply(): Long = go(item.pattern.toList, item.checksum)

    private def go(cs: List[Char], rs: List[Int]): Long = goCache
      .getOrElseUpdate((cs, rs), {
        cs match {
          case List() => if (rs.isEmpty) 1 else 0
          case '.' :: rest => go(rest, rs)
          case '#' :: _ => advance(cs, rs)
          case _ => go(cs.tail, rs) + advance(cs, rs)
        }
      })

    private def advance(cs: List[Char], rs: List[Int]): Long = advanceCache
      .getOrElseUpdate((cs, rs), {
        rs match {
          case List() => 0
          case r :: rest =>
            if (cs.length < r) 0
            else if (cs.take(r) contains '.') 0
            else if (cs.length == r) go(cs.drop(r), rest)
            else if (cs.drop(r).head == '#') 0
            else go(cs.drop(r + 1), rest)
        }
      })
  }

  def step1(input: Input): Long = input.map(_.variantCount).sum

  def step2(input: Input): Long = input.map(_.expanded.variantCount).sum

  def decode(line: String): Item = {
    val parts = line.split(' ')
    val pattern = parts.head
    if (!pattern.forall("?#." contains _)) Oops(s"Bad pattern: $pattern")
    Item(pattern, parts.last.split(',').toList.map(_.toInt))
  }

  lazy val input: Input = me.astynax.Input.linesFrom("Day12.input").map(decode)
}
