package me.astynax

import scala.annotation.tailrec
import scala.collection.mutable

object Day17 {

  def step1(input: Input): Int = search(input, Light)

  def step2(input: Input): Int = search(input, Heavy)

  case class Input(map: Map[Pos, Int], width: Int, height: Int)

  private case class Cursor(pos: Pos, dir: Char, line: Int)

  private trait Strategy {
    def minimumReached(cursor: Cursor): Boolean

    def fork(cursor: Cursor): List[Cursor]
  }

  private object Strategy {
    private val turns: Map[Char, (Char, Char)] = Map(
      '^' -> ('<', '>'),
      '<' -> ('v', '^'),
      'v' -> ('>', '<'),
      '>' -> ('^', 'v'),
    )

    private def move(pos: Pos, dir: Char): Pos = dir match {
      case '^' => pos.move(dy = -1)
      case 'v' => pos.move(dy = 1)
      case '<' => pos.move(dx = -1)
      case '>' => pos.move(dx = 1)
    }

    def leftAndRight(cursor: Cursor): List[Cursor] = {
      val (l, r) = turns(cursor.dir)
      List(
        Cursor(move(cursor.pos, l), l, 0),
        Cursor(move(cursor.pos, r), r, 0),
      )
    }

    def forward(cursor: Cursor): List[Cursor] = List(
      Cursor(move(cursor.pos, cursor.dir), cursor.dir, cursor.line + 1)
    )
  }

  private object Light extends Strategy {
    def fork(cursor: Cursor): List[Cursor] = {
      Strategy.leftAndRight(cursor) ++ (
        if (cursor.line < 2) Strategy.forward(cursor)
        else List()
        )
    }

    def minimumReached(cursor: Cursor): Boolean = true
  }

  private object Heavy extends Strategy {
    def fork(cursor: Cursor): List[Cursor] = {
      if (cursor.line < 3) Strategy.forward(cursor)
      else Strategy.leftAndRight(cursor) ++ (
        if (cursor.line < 9) Strategy.forward(cursor)
        else List()
        )
    }

    def minimumReached(cursor: Cursor): Boolean = cursor.line >= 4
  }

  private def search(input: Input, strategy: Strategy): Int = {
    val finish = Pos(input.width - 1, input.height - 1)
    val cache = mutable.Map.empty[Cursor, Int]
    val paths = mutable.ListBuffer.empty[(Int, List[Pos])]

    @tailrec
    def walk(ps: List[(Cursor, Int, List[Pos])]): Unit = if (ps.nonEmpty) {
      val ns = for {
        (cursor, dist, path) <- ps
        newPath = cursor.pos :: path
        next <- strategy.fork(cursor)
        step <- input.map.get(next.pos).toList
        newDist = dist + step
        if cache.getOrElse(next, Int.MaxValue) > newDist
        _ = cache.update(next, newDist)
        _ = if (next.pos == finish & strategy.minimumReached(next)) {
          paths.append(newDist -> newPath)
        }
      } yield (next, newDist, newPath)
      walk(ns.sortBy(_._2).take(10000)) // some empirically decided limit!
    }

    walk(ps = List(
      (Cursor(Pos(0, 0), '>', 0), 0, List()),
      (Cursor(Pos(0, 0), 'v', 0), 0, List()),
    ))

    paths.map(_._1).min
  }

  def decode(lines: List[String]): Input = Input(
    map = (for {
      (row, y) <- lines.zipWithIndex
      (cell, x) <- row.zipWithIndex
      v = List(cell).mkString.toInt
    } yield Pos(x, y) -> v).toMap,
    width = lines.head.length,
    height = lines.length
  )

  lazy val input: Input = decode(me.astynax.Input.linesFrom("Day17.input"))
}
