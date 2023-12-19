package me.astynax

import scala.annotation.tailrec

object Lists {
  def transpose[A](cells: List[List[A]]): List[List[A]] = {
    @tailrec
    def go(acc: List[List[A]], rests: List[List[A]]): List[List[A]] =
      if (rests.head.isEmpty) acc.reverse
      else go(
        rests.map(_.head) :: acc,
        rests.map(_.tail)
      )
    go(List(), cells)
  }

  def splitBy[A](items: Iterable[A])(by: A => Boolean): Iterable[List[A]] =
    Iterable.unfold[List[A], Iterator[A]](items.iterator) { i =>
      if (i.hasNext) Some(i.takeWhile(!by(_)).toList -> i)
      else None
    }

  def pairwise[A](xs: Iterable[A]): List[(A, A)] =
    xs.sliding(2).map { l => l.head -> l.last }.toList

  @tailrec
  def firstSome[A, B](items: List[A])(f: A => Option[B]): Option[B] =
    items match {
      case x :: xs => f(x) match {
        case None => firstSome(xs)(f)
        case v => v
      }
      case _ => None
    }
}
