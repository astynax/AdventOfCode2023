package me.astynax

case class Pos(x: Int, y: Int) {
  def neighbours: List[Pos] = List(
    Pos(x - 1, y - 1),
    Pos(x,     y - 1),
    Pos(x + 1, y - 1),
    Pos(x - 1, y    ),
    Pos(x + 1, y    ),
    Pos(x - 1, y + 1),
    Pos(x    , y + 1),
    Pos(x + 1, y + 1),
  )

  def neighbours4: List[Pos] = List(
    Pos(x, y - 1),
    Pos(x - 1, y),
    Pos(x + 1, y),
    Pos(x, y + 1),
  )

  def move(dx: Int = 0, dy: Int = 0): Pos = Pos(x + dx, y + dy)

  def move(shift: (Int, Int)): Pos = shift match {
    case (dx, dy) => move(dx, dy)
  }

  def diff(other: Pos): (Int, Int) = (x - other.x, y - other.y)
}
