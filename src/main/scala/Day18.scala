package me.astynax

import scala.annotation.tailrec
import scala.collection.mutable

object Day18 {
  type Input = List[Item]

  private class Grid(path: List[BPos]) {
    private val xs: List[BigInt] = path.map(_.x).sorted.distinct
    private val ys: List[BigInt] = path.map(_.y).sorted.distinct

    private type CoordZipper = Zipper[BigInt, BigInt]
    type RangeZipper = Zipper[(BigInt, BigInt), (BigInt, BigInt)]

    def zipper: Option[CoordZipper] = for {
      xz <- ListZipper.fromList(xs)
      yz <- ListZipper.fromList(ys)
    } yield Zipper(xz, yz)

    def rangeZipper: Option[RangeZipper] = for {
      xz <- ListZipper.fromList(Lists.pairwise(xs))
      yz <- ListZipper.fromList(Lists.pairwise(ys))
    } yield Zipper(xz, yz)

    def rangeMap: Map[Pos, ((BigInt, BigInt), (BigInt, BigInt))] = (for {
      (rx, x) <- Lists.pairwise(xs).zipWithIndex
      (ry, y) <- Lists.pairwise(ys).zipWithIndex
    } yield Pos(x, y) -> (rx, ry)).toMap
  }

  case class Zipper[A, B] private(xz: ListZipper[A], yz: ListZipper[B]) {
    def find(testX: A => Boolean, testY: B => Boolean): Option[Zipper[A, B]] = for {
      x <- xz.find(testX)
      y <- yz.find(testY)
    } yield Zipper(x, y)

    def up: Option[Zipper[A, B]] = yz.back.map { y => copy(yz = y) }

    def down: Option[Zipper[A, B]] = yz.forward.map { y => copy(yz = y) }

    def left: Option[Zipper[A, B]] = xz.back.map { x => copy(xz = x) }

    def right: Option[Zipper[A, B]] = xz.forward.map { x => copy(xz = x) }

    def focus: (A, B) = (xz.focus, yz.focus)

    def focusWith[C](f: (A, B) => C): C = f(xz.focus, yz.focus)

    def pos: Pos = Pos(xz.position, yz.position)
  }

  private def trace(input: Input): List[BPos] = input
    .foldLeft(BPos(0, 0) -> List(BPos(0, 0))) { (acc, step) =>
      val (p, ps) = acc
      val n = step.dir match {
        case 'U' => p.move(dy = -step.length)
        case 'D' => p.move(dy = step.length)
        case 'L' => p.move(dx = -step.length)
        case _   => p.move(dx = step.length)
      }
      (n, n :: ps)
    }._2

  private def collectEdges(grid: Grid, path: List[BPos]): mutable.Set[(BPos, BPos)] = {
    val edges = mutable.Set.empty[(BPos, BPos)]
    val start = grid.zipper.get
      .find({_ == path.head.x}, {_ == path.head.y}).get

    @tailrec
    def go(z: Zipper[BigInt, BigInt], steps: List[BPos]): Unit = steps match {
      case s :: rest =>
        val focus = z.focusWith(BPos.fromPair)
        if (focus == s) go(z, rest) else {
          val n = (
            if (focus.x < s.x) z.right
            else if (focus.x > s.x) z.left
            else if (focus.y < s.y) z.down
            else if (focus.y > s.y) z.up
            else None
            ).getOrElse { Oops(s"Strange state: $z, $s") }
          edges.add(focus to n.focusWith(BPos.fromPair))
          go(n, steps)
        }
      case _ => ()
    }

    go(start, path)

    edges
  }

  def step1(input: Input): BigInt = solve(input)

  def step2(input: Input): BigInt = solve(input.map(_.fixed))

  private def solve(input: Input): BigInt = {
    val path = trace(input)
    val grid = new Grid(path)
    val edges = collectEdges(grid, path)

    val minX = path.map(_.x).min
    val someY = path.filter(_.x == minX).map(_.y).min
    val start = grid.rangeZipper.get.find(
      { case (f, _) => f == minX },
      { case (f, _) => f == someY },
    ).get

    def ways(z: grid.RangeZipper): List[grid.RangeZipper] = {
      val ((fx, tx), (fy, ty)) = z.focus
      List(
        if (!edges.contains(BPos(fx, fy) to BPos(fx, ty))) z.left.toList else List(),
        if (!edges.contains(BPos(tx, fy) to BPos(tx, ty))) z.right.toList else List(),
        if (!edges.contains(BPos(fx, fy) to BPos(tx, fy))) z.up.toList else List(),
        if (!edges.contains(BPos(fx, ty) to BPos(tx, ty))) z.down.toList else List(),
      ).flatten
    }

    val filled = mutable.Set.empty[Pos]
    filled.add(start.pos)

    @tailrec
    def fill(zs: Set[grid.RangeZipper]): Unit = if (zs.nonEmpty) {
      val ns = zs.toSeq.flatMap(ways).filter { z => !(filled contains z.pos) }.toSet
      ns.foreach { z => filled.add(z.pos) }
      fill(ns)
    }

    fill(Set(start))

    val rm = grid.rangeMap
    val inner = filled.toSeq.map { p => rm(p) match {
      case ((fx, tx), (fy, ty)) => (tx - fx) * (ty - fy)
    }}.sum

    input.map(_.length).sum / 2 + inner + 1
  }

  case class Item(dir: Char, length: Int, color: String) {
    def fixed: Item = {
      val l = Integer.parseInt(color.drop(1).dropRight(1), 16)
      val d = color.last match {
        case '0' => 'R'
        case '1' => 'D'
        case '2' => 'L'
        case '3' => 'U'
      }
      Item(d, l, "")
    }
  }

  def decode(line: String): Item = line.split(' ').toList match {
    case List(d, l, c) => Item(d.head, l.toInt, c.drop(1).dropRight(1))
    case _ => Oops(s"Bad line: $line")
  }

  lazy val input: Input = Input.linesFrom("Day18.input").map(decode)
}
