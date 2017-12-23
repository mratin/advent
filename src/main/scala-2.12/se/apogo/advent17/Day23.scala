package se.apogo.advent17

import scala.io.Source
import scala.util.Try

object Day23_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input23.txt").getLines.toSeq

  case class State(reg: Map[String, Long], pc: Long) {
    def next: State = copy(pc = pc + 1)
    def updated(x: String, y: Long): State = copy(reg = reg.updated(x, y))
    def value(x: String): Long = reg.getOrElse(x, 0)
  }

  sealed trait Number { def value(state: State): Long }
  case class Literal(n: Long) extends Number {
    override def value(state: State): Long = n
  }
  case class Register(x: String) extends Number {
    override def value(state: State): Long = state.value(x)
  }

  sealed trait Inst { def apply(state: State): State }
  case class Set(x: String, y: Number) extends Inst {
    override def apply(state: State) = state.updated(x, y.value(state)).next
  }
  case class Add(x: String, y: Number) extends Inst {
    override def apply(state: State) = state.updated(x,state.value(x) + y.value(state)).next
  }
  case class Sub(x: String, y: Number) extends Inst {
    override def apply(state: State) = state.updated(x,state.value(x) - y.value(state)).next
  }
  case class Mul(x: String, y: Number) extends Inst {
    override def apply(state: State) = state.updated(x,state.value(x) * y.value(state)).next
  }
  case class Jnz(x: Number, y: Number) extends Inst {
    override def apply(state: State) = {
      if (x.value(state) != 0) {
        state.copy(pc = state.pc + y.value(state))
      } else {
        state.next
      }
    }
  }

  def parseNumber(x: String): Number = {
    Try(x.toLong).map(Literal).getOrElse(Register(x))
  }

  def parseInst(string: String): Inst = {
    string.split("\\s").map(_.trim) match {
      case Array("set", x, y) => Set(x, parseNumber(y))
      case Array("add", x, y) => Add(x, parseNumber(y))
      case Array("sub", x, y) => Sub(x, parseNumber(y))
      case Array("mul", x, y) => Mul(x, parseNumber(y))
      case Array("jnz", x, y) => Jnz(parseNumber(x), parseNumber(y))
    }
  }

  val program = input.map(parseInst).toIndexedSeq

  def run(state: State, count: Int): Int = {
    if (!program.isDefinedAt(state.pc.toInt)) {
      count
    } else {
      program(state.pc.toInt) match {
        case Mul(a,b) => run(Mul(a,b)(state), count+1)
        case inst => run(inst(state), count)
      }
    }
  }

  val result = run(State(Map.empty, 0), 0)

  println(result)
}

object Day23_2 extends App {

  // I solved this problem by translating the assembly to scala,
  // and then optimize that program
  val inputProgram = {
    // The input program translated to scala:
    var b = 0
    var d = 0
    var e = 0
    var f = 0
    var h = 0

    b = 105700

    do { // for (b <- 105700 to 122700 by 17)
      f = 1
      d = 2

      do { // for (d <- 2 to b)
        e = 2

        do { // for (e <- 2 to b)
          if (d * e == b)
            f = 0

          e = e + 1
        } while (d * e <= b && e < b) // optimization: d * e <= b, enough to get a result

        d = d + 1
      } while (f == 1 && d < b) // optimization: f == 1, not necessary, but faster

      if (f == 0)
        h = h + 1

      b = b + 17
    } while (b <= 122700)

    h
  }

  // Refactored and optimized to functional code:
  val refactored = (105700 to 122700 by 17).count(b => (2 until b).exists(d => b % d == 0))

  println(inputProgram)
  println(refactored)
}
