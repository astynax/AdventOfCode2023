package me.astynax

import scala.annotation.tailrec

object Day22 {
  type Input = List[Piece]

  case class Piece(id: Id, base: Set[Pos], z: Int, height: Int)

  case class Id(id: Int)

  def step1(input: Input): Int = {
    val (ps, ts) = dropAll(input.sortBy(_.z))
    val numOfSupports = ts.values.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
    ps.map(_.id).count { id =>
      ts.get(id).forall { ids =>
        ids.forall(numOfSupports(_) > 1)
      }
    }
  }

  def step2_(input: Input): Int = {
    val (ns, _) = dropAll(input.sortBy(_.z))
    val s = ns.toSet
    (for {
      x <- ns
      n = (s -- dropAll(ns.filter(_.id != x.id))._1).size - 1
    } yield n).sum
  }

  private def dropAll(input: Input): (List[Piece], Map[Id, Set[Id]]) = {
    val (ps, _, touches) = input.foldLeft((
      List.empty[Piece],
      Map.empty[Pos, (Id, Int)],
      Map.empty[Id, Set[Id]],
    )) { (acc, piece) =>
      val (ps, surface, touches) = acc
      val tops = piece.base.flatMap { pos => surface.get(pos).toList }
      if (tops.isEmpty) (
        // put onto the bottom
        piece.copy(z = 1) :: ps,
        surface ++ piece.base.map(_ -> (piece.id, piece.height)),
        touches
      ) else {
        // put onto some obstacles
        val topZ = tops.map(_._2).max
        val topZIds = for {
          (id, z) <- tops
          if z == topZ
        } yield id
        val newPiece = piece.copy(z = topZ + 1)
        (
          newPiece :: ps,
          newPiece.base.foldLeft(surface) { (acc, pos) =>
            acc.updated(pos, (newPiece.id, newPiece.z + newPiece.height - 1))
          },
          topZIds.foldLeft(touches) { (acc, id) =>
            acc.updatedWith(id) { v =>
              v.map(_ + newPiece.id)
                .orElse(Some(Set(newPiece.id)))
            }
          }
        )
      }
    }
    (ps.reverse, touches)
  }

  private def decodeTriple(s: String): (Int, Int, Int) = {
    val x :: y :: z :: _ = s.split(',').toList.map(_.toInt)
    (x, y, z)
  }

  private def decodePiece(idx: Int, line: String): Piece = {
    val (x1, y1, z1) :: (x2, y2, z2) :: _ = line.split('~').toList.map(decodeTriple)
    val base = (for {
      x <- (x1 min x2) to (x1 max x2)
      y <- (y1 min y2) to (y1 max y2)
    } yield Pos(x, y)).toSet
    Piece(
      id = Id(idx),
      base = base,
      z = z1 min z2,
      height = scala.math.abs(z1 - z2) + 1,
    )
  }

  def decode(lines: List[String]): Input = lines
    .zipWithIndex.map { case (l, i) => decodePiece(i, l) }

  lazy val input: Input = decode(Input.linesFrom("Day22.input"))
}
