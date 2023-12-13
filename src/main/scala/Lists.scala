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
}
