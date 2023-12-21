package me.astynax

import scala.annotation.tailrec

object Day20 {

  private type Circuit = Map[Name, CircuitModule]

  case class Input(modules: Circuit, broadcastTo: List[Name])

  case class Name(name: String)

  private val Broadcaster: Name = Name("broadcaster")

  object Signal extends Enumeration {
    type Signal = Value
    val Hi, Low = Value
  }

  import Signal._

  trait CircuitModule {
    val outputs: List[Name]

    def propagate(signal: Signal, from: Name): (CircuitModule, List[(Signal, Name)])
  }

  case class FF(outputs: List[Name], state: Boolean = false) extends CircuitModule {
    override def propagate(signal: Signal, from: Name): (CircuitModule, List[(Signal, Name)]) = signal match {
      case Hi => this -> List()
      case Low =>
        val newState = !state
        val s = if (newState) Hi else Low
        copy(state = newState) -> outputs.map(s -> _)
    }
  }

  case class NAND(outputs: List[Name], state: Map[Name, Signal]) extends CircuitModule {
    override def propagate(signal: Signal, from: Name): (CircuitModule, List[(Signal, Name)]) = {
      val newState = state.updated(from, signal)
      val s = if (newState.values.forall(_ == Hi)) Low else Hi
      copy(state = newState) -> outputs.map(s -> _)
    }
  }

  private case class Message(from: Name, to: Name, signal: Signal) {
    def readable: String = s"${from.name} -${if (signal == Low) "low" else "high"}-> ${to.name}"
  }

  def step1(input: Input): BigInt = pushButton(input, 1000)

  def pushButton(input: Input, times: Int, debug: Boolean = false): Int = {
    val (_, (l, h)) = (1 to times)
      .foldLeft((input.modules, (0, 0))) { (acc, _) =>
        val (circuit, (lows, highs)) = acc
        if (debug) println()
        flow[(Int, Int)](
          circuit,
          input.broadcastTo.map { name => Message(Broadcaster, name, Low) },
          (lows + 1, highs),
          { (msg, acc) =>
            val (l, h) = acc
            if (debug) println(msg.readable)
            msg.signal match {
              case Hi => (l, h + 1)
              case Low => (l + 1, h)
            }
          }
        )
      }
    l * h
  }

  def step2(input: Input): BigInt = {
    // I don't have a module named "rx" but it has just a single source "qn"
    val sources = input.modules(Name("qn")).asInstanceOf[NAND].state.keys.toSet

    @tailrec
    def iterate(circuit: Circuit,
                keys: Set[Name],
                counts: Map[Name, BigInt],
                step: BigInt): Map[Name, BigInt] = {
      if (keys.isEmpty) counts
      else {
        val (c, (ks, cs)) = flow[(Set[Name], Map[Name, BigInt])](
          circuit,
          input.broadcastTo.map { name => Message(Broadcaster, name, Low) },
          (keys, counts),
          { (msg, acc) =>
            val (ks, cs) = acc
            if (msg.signal == Low & (ks contains msg.to))
              (ks - msg.to, cs.updated(msg.to, step))
            else acc
          }
        )
        iterate(c, ks, cs, step + 1)
      }
    }

    val counts = iterate(input.modules, sources, Map.empty, 1)

    Math.lcm(counts.values)
  }

  @tailrec
  private def flow[A](circuit: Circuit,
                      messages: List[Message],
                      extra: A,
                      extraStep: (Message, A) => A,
                     ): (Circuit, A) = {
    if (messages.isEmpty) (circuit, extra)
    else {
      val (newCircuit, nss, newExtra) = messages
        .foldLeft((circuit, List.empty[Message], extra)) { (acc, message) =>
          val (circ, outbox, ex) = acc
          val newExtra = extraStep(message, ex)
          circ.get(message.to) match {
            case None => (circ, outbox, newExtra)
            case Some(mod) =>
              val (moduleState, outputSignals) = mod.propagate(message.signal, message.from)
              val newOutbox = outbox ++ outputSignals.map { case (s, t) => Message(message.to, t, s) }
              (circ.updated(message.to, moduleState), newOutbox, newExtra)
          }
        }
      flow(newCircuit, nss, newExtra, extraStep)
    }
  }

  def decode(lines: List[String]): Input = {
    val pairs = lines.map { line =>
      line.split(" -> ", 2).toList match {
        case List(k, v) => k -> v.split(", ").toList.map(Name)
        case _ => Oops(s"Bad line: $line")
      }
    }
    val bc = pairs.find { case (n, _) => n == Broadcaster.name }.get._2
    val ms = pairs.filter { case (n, _) => n != Broadcaster.name }
      .map { case (n, ns) =>
        Name(n.tail) -> (n.head match {
          case '%' => FF(ns)
          case '&' => NAND(ns, Map.empty)
          case _ => Oops(s"Bad module $n")
        })
      }
    // input linking
    val us = ms.flatMap { case (n, mod) => mod.outputs.map(_ -> n) }
    val circuit = us.foldLeft(ms.toMap) { (acc, update) =>
      val (t, f) = update
      acc.updatedWith(t)(_.map {
        case mod@FF(_, _) => mod
        case mod@NAND(_, state) => mod.copy(state = state.updated(f, Low))
      })
    }
    Input(circuit, bc)
  }

  lazy val input: Input = decode(me.astynax.Input.linesFrom("Day20.input"))
}
