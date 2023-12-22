package me.astynax

object Implicits {
  implicit class NumericOps[A](that: A) {
    def inc(implicit numeric: Numeric[A]): A = numeric.plus(that, numeric.fromInt(1))

    def dec(implicit numeric: Numeric[A]): A = numeric.minus(that, numeric.fromInt(1))
  }
}
