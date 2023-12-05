package me.astynax

object Lists {
  def splitBy[A](items: Iterable[A])(by: A => Boolean): Iterable[List[A]] =
    Iterable.unfold[List[A], Iterator[A]](items.iterator) { i =>
      if (i.hasNext) Some(i.takeWhile(!by(_)).toList -> i)
      else None
    }
}
