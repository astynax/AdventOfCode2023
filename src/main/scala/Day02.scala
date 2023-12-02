package me.astynax

import fastparse._
import NoWhitespace._

import javax.print.attribute.standard.MediaSize.Other

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

  private def pGame[_: P]: P[Game] =
    for {
      _ <- P("Game ")
      id <- pNumber
      _ <- P(": ")
      cs <- pCubes.rep(min=1, sep="; ").map(_.toList)
      _ <- End
    } yield (id, cs)

  private def pCubes[_: P]: P[Cubes] = {
    (for {
      n <- pNumber
      _ <- P(" ")
      color <- P(("red" | "green" | "blue").!)
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

  private def pNumber[_: P]: P[Int] = P(CharPred(_.isDigit).rep.!).map(Integer.parseInt)
}
