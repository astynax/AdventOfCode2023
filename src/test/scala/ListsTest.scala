package me.astynax

import org.scalatest.funsuite.AnyFunSuiteLike

class ListsTest extends AnyFunSuiteLike {
  test("splitBy") {
    assert(
      Lists.splitBy(List(1, 2, 0, 3, 4))(_ == 0).take(3) == List(List(1, 2), List(3, 4))
    )
  }
}
