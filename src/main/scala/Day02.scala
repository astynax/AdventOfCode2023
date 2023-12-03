package me.astynax

import fastparse._
import NoWhitespace._

object Day02 {
  type Input = List[Game]
  type Game = (Int, List[Cubes])

  case class Cubes(r: Int, g: Int, b: Int) {
    def <=(other: Cubes): Boolean = r <= other.r & g <= other.g & b <= other.b

    def max(other: Cubes): Cubes = Cubes(r max other.r, g max other.g, b max other.b)
  }

  private val bag: Cubes = Cubes(12, 13, 14)

  def step1(games: Input): Int =
    games.filter { case (_, cs) => cs.forall(_ <= bag) }
      .map(_._1).sum

  def step2(games: Input): Int =
    games.map { case (_, cs) =>
      cs.foldLeft(Cubes(1, 1, 1)) { (acc, c) => acc max c }
    }.map { case Cubes(r, g, b) => r * g * b }.sum

  lazy val input: Input = Input.linesFrom("Day02.input").map(parseGame)

  def parseGame(string: String): Game = parse(string, pGame(_)).get.value

  private def pGame[$: P]: P[Game] =
    for {
      _ <- P("Game ")
      id <- pNumber
      _ <- P(": ")
      cs <- pTakes
      _ <- End
    } yield (id, cs)

  private def pTakes[$: P]: P[List[Cubes]] =
    pCubes.rep(min = 1, sep = "; ").map(_.toList)

  private def pCubes[$: P]: P[Cubes] = {
    (for {
      n <- pNumber
      _ <- P(" ")
      color <- pRGB
    } yield (n, color))
      .rep(min=1, max=3, sep=", ")
      .map {
        _.foldLeft(Cubes(0, 0, 0)) { (acc, take) =>
          take match {
            case (r, "red") => acc.copy(r=r)
            case (g, "green") => acc.copy(g=g)
            case (b, "blue") => acc.copy(b=b)
          }
        }
      }
  }

  private def pRGB[$: P]: P[String] = P(("red" | "green" | "blue").!)

  private def pNumber[$: P]: P[Int] = CharsWhile(_.isDigit, 1).!.map(_.toInt)
}
