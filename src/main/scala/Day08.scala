package me.astynax

import scala.annotation.tailrec

object Day08 {
  type Input = (List[RL], Graph)
  type Graph = Map[Node, (Node, Node)]

  type RL = Either[Unit, Unit]
  val R: RL = Right(())
  val L: RL = Left(())

  case class Node(name: String)

  def step1: Input => BigInt = walk(Node("AAA"), { _ == Node("ZZZ") })

  def step2: Input => BigInt = {
    case (path, graph) =>
      val starts = graph.keys.filter { _.name.endsWith("A") }.toList
      val cycles = starts.map(walk(_, { _.name.endsWith("Z") }, fullLoop = true)(path, graph))
      Math.lcm(cycles)
  }

  private def walk(start: Node,
                   atFinish: Node => Boolean,
                   fullLoop: Boolean = false): Input => BigInt = {

    case (path, graph) =>
      @tailrec
      def step(xs: List[RL], here: Node, n: BigInt): BigInt =
        if (atFinish(here) & (!fullLoop | xs.isEmpty)) n
        else {
          xs match {
            case x :: rest => step(rest, {
              val (l, r) = graph(here)
              x.fold({ _ => l }, { _ => r })
            }, n + 1)
            case _ => step(path, here, n)
          }
        }

      step(path, start, 0)

  }

  lazy val input: Input = decode(Input.linesFrom("Day08.input"))

  def decode: List[String] => Input = {
    case steps :: _ :: rest =>
      val path = steps.map {
        case 'R' => R
        case 'L' => L
        case _ => Oops("Bad path!")
      }.toList
      val graph = rest.map { line =>
          line.split(' ').toList match {
            case List(k, _, l, r) => Node(k) -> (
              Node(l.drop(1).dropRight(1)),
              Node(r.dropRight(1))
            )
            case _ => Oops(s"Bad line: $line")
          }
        }.toMap
      (path, graph)
    case _ => Oops("Bad input!")
  }

}
