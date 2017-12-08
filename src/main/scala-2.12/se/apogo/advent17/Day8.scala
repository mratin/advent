package se.apogo.advent17

import scala.io.Source

object Day8_1 extends App {
  val input: List[String] = Source.fromResource("se/apogo/advent17/input08.txt").getLines.toList

  case class V(name: String)

  sealed trait Op
  case object Lt extends Op
  case object Gt extends Op
  case object Lte extends Op
  case object Gte extends Op
  case object Neq extends Op
  case object Eq extends Op

  case class Cond(a: V, b: Int, op: Op) {
    def isSatisfied(state: State): Boolean = op match {
      case Lt => state.value(a) < b
      case Gt => state.value(a) > b
      case Lte => state.value(a) <= b
      case Gte => state.value(a) >= b
      case Neq => state.value(a) != b
      case Eq => state.value(a) == b
    }
  }
  case class Inst(v: V, action: Int => Int, cond: Cond)
  case class State(values: Map[V, Int]) {
    def value(v: V): Int = values.getOrElse(v,0)
  }

  def parseInst(line: String): Inst = {
    val split = line.split("\\s").map(_.trim)
    val value = Integer.parseInt(split(2))
    val action: (Int) => Int = split(1) match {
      case "inc" => v: Int => v + value
      case "dec" => v: Int => v - value
    }

    val op = split(5) match {
      case "<" => Lt
      case ">" => Gt
      case "<=" => Lte
      case ">=" => Gte
      case "!=" => Neq
      case "==" => Eq
    }

    val cond = Cond(V(split(4)), Integer.parseInt(split(6)), op)

    Inst(V(split(0)), action, cond)
  }

  def run(is: List[Inst], state: State): State = {
    is match {
      case Nil => state
      case inst::rest =>
        if (inst.cond.isSatisfied(state)) {
          run(rest, State(state.values.updated(inst.v, inst.action(state.value(inst.v)))))
        } else {
          run(rest, state)
        }
    }
  }

  val endState = run(input.map(parseInst), State(Map.empty))
  val result = endState.values.values.max

  println(result)
}


object Day8_2 extends App {
  val input: List[String] = Source.fromResource("se/apogo/advent17/input08.txt").getLines.toList

  case class V(name: String)

  sealed trait Op
  case object Lt extends Op
  case object Gt extends Op
  case object Lte extends Op
  case object Gte extends Op
  case object Neq extends Op
  case object Eq extends Op

  case class Cond(a: V, b: Int, op: Op) {
    def isSatisfied(state: State): Boolean = op match {
      case Lt => state.value(a) < b
      case Gt => state.value(a) > b
      case Lte => state.value(a) <= b
      case Gte => state.value(a) >= b
      case Neq => state.value(a) != b
      case Eq => state.value(a) == b
    }
  }
  case class Inst(v: V, action: Int => Int, cond: Cond)
  case class State(values: Map[V, Int]) {
    def value(v: V): Int = values.getOrElse(v,0)
  }

  def parseInst(line: String): Inst = {
    val split = line.split("\\s").map(_.trim)
    val value = Integer.parseInt(split(2))
    val action: (Int) => Int = split(1) match {
      case "inc" => v: Int => v + value
      case "dec" => v: Int => v - value
    }

    val op = split(5) match {
      case "<" => Lt
      case ">" => Gt
      case "<=" => Lte
      case ">=" => Gte
      case "!=" => Neq
      case "==" => Eq
    }

    val cond = Cond(V(split(4)), Integer.parseInt(split(6)), op)

    Inst(V(split(0)), action, cond)
  }

  def run(is: List[Inst], state: State, max: Int): Int = {
    is match {
      case Nil => max
      case inst::rest =>
        if (inst.cond.isSatisfied(state)) {
          val newState = State(state.values.updated(inst.v, inst.action(state.value(inst.v))))
          run(rest, newState, math.max(newState.values.values.max, max))
        } else {
          run(rest, state, max)
        }
    }
  }

  val result = run(input.map(parseInst), State(Map.empty), 0)

  println(result)
}
