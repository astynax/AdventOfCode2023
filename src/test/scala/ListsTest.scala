package me.astynax

import org.scalatest.funsuite.AnyFunSuiteLike

class ListsTest extends AnyFunSuiteLike {
  test("splitBy") {
    assert(
      Lists.splitBy(List(1, 2, 0, 3, 4))(_ == 0).take(3) == List(List(1, 2), List(3, 4))
    )
  }

  test("transposing") {
    assert(Lists.transpose(List(List(1, 2), List(3, 4), List(5, 6))) == List(
      List(1, 3, 5),
      List(2, 4, 6),
    ))
  }

  test("pairwise") {
    assert(Lists.pairwise("asdf") == List(
      ('a', 's'), ('s', 'd'), ('d', 'f'),
    ))
  }
}
