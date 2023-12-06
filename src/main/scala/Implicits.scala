package me.astynax

object Implicits {
  implicit class OrderedOps[A](that: A) {
    def min(other: A)(implicit ord: A => Ordered[A]): A =
      if (that <= other) that else other

    def max(other: A)(implicit ord: A => Ordered[A]): A =
      if (that >= other) that else other
  }

  implicit class NumericOps[A](that: A) {
    def inc(implicit numeric: Numeric[A]): A = numeric.plus(that, numeric.fromInt(1))

    def dec(implicit numeric: Numeric[A]): A = numeric.minus(that, numeric.fromInt(1))
  }
}
