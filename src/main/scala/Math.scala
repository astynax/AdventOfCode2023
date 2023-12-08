package me.astynax

import scala.annotation.tailrec

object Math {
  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)

  def lcm(list: Iterable[BigInt]): BigInt =
    list.foldLeft(BigInt(1)) { (a, b) =>
      (a / gcd(a, b)) * b
    }
}
