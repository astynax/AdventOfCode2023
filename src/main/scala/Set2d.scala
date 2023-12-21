package me.astynax

object Set2d {
  def dump(points: Set[Pos], mkPos: (Int, Int) => Pos = Pos): Unit = {
    val xs = points.map(_.x)
    val xr = xs.min to xs.max
    val ys = points.map(_.y)
    (ys.min to ys.max).foreach { y =>
      println(xr.map { x =>
        if (points contains mkPos(x, y)) '.' else '#'
      }.mkString)
    }
  }
}
