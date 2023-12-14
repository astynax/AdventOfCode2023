package me.astynax

import me.astynax

import scala.annotation.tailrec
import scala.collection.mutable

object Day14 {
  type Input = Platform

  def step1(i: Input): Long = i.tilt.load

  def step2(input: Input): Long = {
    val vs: mutable.Set[Set[Pos]] = mutable.Set.empty

    @tailrec
    def findLoop(p: Platform, n: Long = 0): (Long, Platform) = {
      val k = p.key
      if (vs contains k) (n, p)
      else {
        vs.add(k)
        findLoop(p.cycle, n + 1)
      }
    }

    val (first, state) = findLoop(input)
    vs.clear()
    val (loop, _) = findLoop(state)
    val off = first - loop
    val rem = scala.math.floorMod(1000000000L - off, loop)

    input.cycles((off + rem).toInt).load
  }

  case class Platform(platform: Map[Pos, Item], width: Int, height: Int) {

    def key: Set[Pos] = platform
      .toSeq.filter { case (_, i) => i == Rock }.map(_._1).toSet

    def load: Long = platform
      .toSeq.filter(_._2 == Rock)
      .map(height - _._1.y)
      .sum

    def cycle: Platform = tiltNorth.tiltWest.tiltSouth.tiltEast

    def cycles(n: Int): Platform =
      (0 until n).foldLeft(this) { (x, _) => x.cycle }

    def tilt: Platform = tiltNorth

    private def tiltNorth: Platform = tiltWith(
      range1 = 0 until width,
      range2 = 0 until height,
      mkSubRange = { y => (y + 1) until height },
      mkPos = Pos
    )

    private def tiltSouth: Platform = tiltWith(
      range1 = (width - 1).to(0, -1),
      range2 = (height - 1).to(0, -1),
      mkSubRange = { y => (y - 1).to(0, -1) },
      mkPos = Pos
    )

    private def tiltWest: Platform = tiltWith(
      range1 = 0 until height,
      range2 = 0 until width,
      mkSubRange = { x => (x + 1) until width },
      mkPos = { (y, x) => Pos(x, y) }
    )

    private def tiltEast: Platform = tiltWith(
      range1 = (height - 1).to(0, -1),
      range2 = (width - 1).to(0, -1),
      mkSubRange = { x => (x - 1).to(0, -1) },
      mkPos = { (y, x) => Pos(x, y) }
    )

    private def tiltWith(range1: Range,
                         range2: Range,
                         mkSubRange: Int => Range,
                         mkPos: (Int, Int) => Pos) = copy(
      platform = range1.foldLeft(platform) { (p, a) =>
        range2.foldLeft(p) { (acc, b) =>
          val pos = mkPos(a, b)
          acc.get(pos) match {
            case Some(_) => acc
            case None =>
              mkSubRange(b)
                .map(mkPos(a, _))
                .dropWhile { t => !acc.contains(t) }
                .headOption
                .map { t =>
                  if (acc.get(t) contains Rock)
                    acc.updated(pos, Rock).removed(t)
                  else acc
                }.getOrElse(acc)
          }
        }
      })
  }

  sealed trait Item
  private case object Rock extends Item
  private case object Block extends Item

  object Item {
    def fromChar: Char => Option[Item] = {
      case '#' => Some(Block)
      case 'O' => Some(Rock)
      case _ => None
    }
  }

  def decode(lines: List[String]): Input = {
    val width = lines.head.length
    val height = lines.length
    val platform = (for {
      (row, y) <- lines.zipWithIndex
      (cell, x) <- row.zipWithIndex
      v <- Item.fromChar(cell)
    } yield Pos(x, y) -> v).toMap
    Platform(platform, width, height)
  }

  lazy val input: Input = decode(me.astynax.Input.linesFrom("Day14.input"))
}
