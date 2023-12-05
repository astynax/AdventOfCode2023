package me.astynax

import org.scalatest.funsuite.AnyFunSuiteLike

import Ranges._

class RangesTest extends AnyFunSuiteLike {
  test("associating") {
    assert(associate((1, 5), (3, 8)) == Overlaps((1, 2), (3, 5), (6, 8)))
    assert(associate((3, 8), (1, 5)) == Overlaps((1, 2), (3, 5), (6, 8)))
    assert(associate((1, 3), (5, 8)) == Disjointed[Int]())
    assert(associate((1, 8), (3, 5)) == Includes(Some((1, 2)), (3, 5), Some((6, 8))))
    assert(associate((3, 5), (1, 8)) == Included(Some((1, 2)), (3, 5), Some((6, 8))))
  }
}
