package me.astynax

case class BPos(x: BigInt, y: BigInt) {

  def neighbours4: List[BPos] = List(
    move(dy = -1),
    move(dy = 1),
    move(dx = -1),
    move(dx = 1),
  )

  def move(dx: BigInt = 0, dy: BigInt = 0): BPos = BPos(x + dx, y + dy)

  def to(other: BPos): (BPos, BPos) = {
    if (x < other.x | (x == other.x) & (y < other.y))
      (this, other)
    else (other, this)
  }
}

object BPos {
  def fromPair: (BigInt, BigInt) => BPos = { case (x, y) => BPos(x, y) }
}
