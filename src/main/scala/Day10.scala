package me.astynax

import scala.annotation.tailrec

import scala.jdk.StreamConverters.StreamHasToScala

object Day10 {
  case class Input(start: Pos, pipes: Map[Pos, Char])

  def step1(input: Input): Int =
    scala.math.ceil(findLoop(input).size.toDouble / 2).toInt

  @tailrec
  private def firstSome[A, B](items: List[A])(f: A => Option[B]): Option[B] =
    items match {
      case x :: xs => f(x) match {
        case None => firstSome(xs)(f)
        case v => v
      }
      case _ => None
    }

  lazy val rules: Map[((Int, Int), (Int, Int)), List[(Int, Int)]] = {
    val raw =
    """.l.|x..
      |.*.|x..
      |.n.|x..
      |
      |.n.|..x
      |.*.|..x
      |.l.|..x
      |
      |...|...
      |l*n|...
      |...|xxx
      |
      |...|xxx
      |n*l|...
      |...|...
      |
      |...|...
      |l*.|...
      |.n.|x..
      |
      |...|xxx
      |n*.|..x
      |.l.|..x
      |
      |...|xxx
      |.*l|x..
      |.n.|x..
      |
      |...|...
      |.*n|...
      |.l.|..x
      |
      |.n.|..x
      |l*.|..x
      |...|xxx
      |
      |.l.|x..
      |n*.|...
      |...|...
      |
      |.n.|..x
      |.*l|...
      |...|...
      |
      |.l.|x..
      |.*n|x..
      |...|xxx
      |""".stripMargin.lines().toScala(List)
    val groups = Lists.splitBy(raw) {_ == ""}

    def index(rows: List[String]) = for {
      (row, y) <- rows zip List(-1, 0, 1)
      (c, x) <- row zip List(-1, 0, 1)
      if !(x == 0 & y == 0) & c != '.'
    } yield c -> (x, y)

    def toPair(group: List[String]) = {
      val (k, v) = group.map { s =>
        val parts = s.split('|')
        parts.head -> parts.last
      }.unzip
      val mk = index(k)
      val mv = index(v)
      (mk.find(_._1 == 'l').get._2,
        mk.find(_._1 == 'n').get._2) -> mv.map(_._2)
    }

    groups.map(toPair).toMap
  }

  def step2(input: Input): Int = {
    val loop = findLoop(input)
    val segments = loop.toSet
    val triples = (
      (loop.last :: loop)
        zip loop
        zip (loop.drop(1) ++ loop.take(1))
      )
    val directions: List[(Pos, Char)] = triples.flatMap {
      case ((a, b), c) =>
        val vertical = a.x == b.x & b.x == c.x
        val horizontal = a.y == b.y & b.y == c.y
        if (vertical) {
          List(b -> (if (a.y < b.y) 'D' else 'U'))
        } else if (horizontal) {
          List(b -> (if (a.x < b.x) 'R' else 'L'))
        } else List()
    }

    @tailrec
    def scan(pos: Pos, dx: Int = 0, dy: Int = 0): Boolean = {
      val p = pos.move(dx, dy)
      if (segments contains p) false
      else if (!(input.pipes contains p)) true
      else scan(p, dx, dy)
    }

    // There is an edge case when all the loops is made from the corners.
    // Then .head will throw because there are no straight segments
    // to deduce the loop direction from.
    // TODO: scan from corners too!
    val clockwise: Boolean = firstSome(directions) { case (p, d) =>
      if (d == 'U' | d == 'D') {
        val s1 = scan(p, dx = -1)
        val s2 = scan(p, dx = 1)
        if (!s1 & !s2) None
        else Some(if (d == 'D') s2 else s1)
      } else {
        val s1 = scan(p, dy = -1)
        val s2 = scan(p, dy = 1)
        if (!s1 & !s2) None
        else Some(if (d == 'R') s1 else s2)
      }
    }.get
    val cwTriples =
      if (clockwise) triples
      else triples.reverse.map { case ((l, p), n) => ((n, p), l) }

    // Inner contour "painting"
    val contoured = cwTriples.foldLeft(input.pipes) { (map, triple) =>
      val ((l, p), n) = triple
      val k = (l diff p, n diff p)
      val ds = rules(k).map(p.move)

      ds.foldLeft(map) { (m, pos) =>
        val c = m.get(pos)
        if ((segments contains pos) | c.isEmpty) m
        else m.updated(pos, 'I')
      }
    }

    @tailrec
    def fill(m: Map[Pos, Char], seen: Set[Pos]): Map[Pos, Char] = {
      val ms = m.toSeq.filter { case (_, c) => c == 'I' }.map(_._1)
      val newSeen = seen ++ ms
      val ps = ms
        .flatMap(_.neighbours4)
        .filter { p => !(newSeen contains p) }
        .filter { p => m contains p }
        .toSet
      if (ps.isEmpty) m
      else fill(
        ps.foldLeft(m) { (acc, p) => acc.updated(p, 'I') },
        newSeen ++ ps
      )
    }
    fill(contoured, segments).toSeq.count { case (_, c) => c == 'I' }
  }

  def printOut(m: Map[Pos, Char]): Unit = {
    val w = m.map { case (Pos(x, _), _) => x }.max
    val h = m.map { case (Pos(_, y), _) => y }.max
    (0 to h).foreach { y =>
      println((0 to w).map { x =>
        m(Pos(x, y))
      }.mkString)
    }
  }

  def findLoop(input: Input): List[Pos] = {
    val ws = waysFrom(input.start, input.pipes)
    input.start :: walk(ws.head, input.pipes, visited = Set(input.start)).reverse
  }

  @tailrec
  def walk(pos: Pos,
           map: Map[Pos, Char],
           visited: Set[Pos] = Set.empty,
           path: List[Pos] = List.empty): List[Pos] = {
    val ps = waysFrom(pos, map)
    val p = ps.find { !visited.contains(_) }
    if (p.isDefined)
      walk(p.get, map, visited + pos, pos :: path)
    else pos :: path
  }

  private def waysFrom(pos: Pos, map: Map[Pos, Char]): List[Pos] = {
    val u = pos.move(dy = -1)
    val d = pos.move(dy = 1)
    val l = pos.move(dx = -1)
    val r = pos.move(dx = 1)
    (map.getOrElse(pos, '.') match {
      case '|' => List(u, d)
      case '-' => List(l, r)
      case 'L' => List(u, r)
      case 'J' => List(u, l)
      case '7' => List(d, l)
      case 'F' => List(d, r)
      case _ => List()
    }).filter(map.contains)
  }

  def decode(lines: List[String]): Input = {
    val m = (for {
      (row, y) <- lines.zipWithIndex
      (c, x) <- row.zipWithIndex
    } yield Pos(x, y) -> c).toMap
    val s = m.toSeq.find { case (_, c) => c == 'S' }.get._1
    def check(cs: String, p: Pos) = cs contains m.getOrElse(p, '?')
    val patch: Char = Map(
      (true, true, false, false) -> '|',
      (false, false, true, true) -> '-',
      (true, false, true, false) -> 'J',
      (true, false, false, true) -> 'L',
      (false, true, false, true) -> 'F',
      (false, true, true, false) -> '7',
    )((
      check("7|F", s.move(dy = -1)), // up
      check("J|L", s.move(dy = 1)), // down
      check("L-F", s.move(dx = -1)), // left
      check("J-7", s.move(dx = 1)), // right
    ))
    Input(s, m + (s -> patch))
  }

  lazy val input: Input = decode(me.astynax.Input.linesFrom("Day10.input"))
}
