package me.astynax

import org.scalatest.funsuite.AnyFunSuiteLike

class ListZipperTest extends AnyFunSuiteLike {
  test("fromList") {
    assert(ListZipper.fromList(List()).isEmpty)
    assert(ListZipper.fromList(List(1)).map(_.focus) contains 1)
  }

  test("left-right") {
    val z = ListZipper.fromList(List(1, 2, 3)).get
    assert(z.forward.get.back.get == z)
  }
}
