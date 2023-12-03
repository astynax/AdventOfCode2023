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

  def move(dx: Int = 0, dy: Int = 0): Pos = Pos(x + dx, y + dy)
}
