package me.astynax

import org.scalatest.funsuite.AnyFunSuiteLike

class InputTest extends AnyFunSuiteLike {

  test("reading lines from") {
    assert(Input.linesFrom("Day01.input").nonEmpty)
  }

}
