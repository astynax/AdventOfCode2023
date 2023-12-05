package me.astynax

object Oops {
  def apply(msg: String): Nothing = throw new IllegalArgumentException(msg)
}
