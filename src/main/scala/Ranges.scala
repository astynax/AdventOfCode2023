package me.astynax

object Ranges {

  private type R[A] = (A, A)

  sealed trait Relation[A]
  case class Disjointed[A]() extends Relation[A]

  case class Overlaps[A](l: R[A], i: R[A], r: R[A]) extends Relation[A]

  case class Includes[A](l: Option[R[A]], i: R[A], r: Option[R[A]]) extends Relation[A]

  case class Included[A](l: Option[R[A]], i: R[A], r: Option[R[A]]) extends Relation[A]

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

  def associate[A: Numeric](r1: R[A], r2: R[A])(
    implicit
    numeric: Numeric[A],
    toOrdered: A => Ordered[A],
    ordering: Ordering[A],
  ): Relation[A] = {
    val (f1, t1) = r1
    val (f2, t2) = r2
    if (t1 < f2 | f1 > t2) Disjointed()
    else {
      val s = List(f1, f2, t1, t2).sorted(ordering)
      if (s == List(f1, f2, t2, t1))
        Includes(
          if (f1 == f2) None else Some[R[A]](f1, f2.dec(numeric)),
          (f2, t2),
          if (t2 == t1) None else Some[R[A]](t2.inc(numeric), t1),
        )
      else if (s == List(f2, f1, t1, t2))
        Included(
          if (f2 == f1) None else Some[R[A]](f2, f1.dec(numeric)),
          (f1, t1),
          if (t1 == t2) None else Some[R[A]](t1.inc(numeric), t2)
        )
      else if (s == List(f1, f2, t1, t2))
        Overlaps((f1, f2.dec(numeric)), (f2, t1), (t1.inc(numeric), t2))
      else
        Overlaps((f2, f1.dec(numeric)), (f1, t2), (t2.inc(numeric), t1))
    }
  }

  def offset[A](r: R[A], off: A)(implicit num: Numeric[A]): R[A] =
    r match { case (a, b) => num.plus(a, off) -> num.plus(b, off) }
}
