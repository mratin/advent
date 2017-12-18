package se.apogo.advent17

import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.Try

object Day18_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input18.txt").getLines.toSeq

  case class State(reg: Map[String, Long], pc: Long, played: Seq[Long], recovered: Seq[Long]) {
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
  case class Snd(x: Number) extends Inst {
    def apply(state: State) = state.copy(played = state.played ++ Seq(x.value(state))).next
  }
  case class Set(x: String, y: Number) extends Inst {
    override def apply(state: State) = state.updated(x, y.value(state)).next
  }
  case class Add(x: String, y: Number) extends Inst {
    override def apply(state: State) = state.updated(x,state.value(x) + y.value(state)).next
  }
  case class Mul(x: String, y: Number) extends Inst {
    override def apply(state: State) = state.updated(x,state.value(x) * y.value(state)).next
  }
  case class Mod(x: String, y: Number) extends Inst {
    override def apply(state: State) = state.updated(x,state.value(x) % y.value(state)).next
  }
  case class Rcv(x: Number) extends Inst {
    override def apply(state: State) = {
      if (x.value(state) != 0) {
        state.copy(recovered = state.recovered :+ state.played.last).next
      } else {
        state.next
      }
    }
  }
  case class Jgz(x: Number, y: Number) extends Inst {
    override def apply(state: State) = {
      if (x.value(state) > 0) {
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
      case Array("snd", x)    => Snd(parseNumber(x))
      case Array("set", x, y) => Set(x, parseNumber(y))
      case Array("add", x, y) => Add(x, parseNumber(y))
      case Array("mul", x, y) => Mul(x, parseNumber(y))
      case Array("mod", x, y) => Mod(x, parseNumber(y))
      case Array("rcv", x)    => Rcv(parseNumber(x))
      case Array("jgz", x, y) => Jgz(parseNumber(x), parseNumber(y))
    }
  }


  def run(program: IndexedSeq[Inst])(state: State): Long = {
    if (state.recovered.nonEmpty) {
      state.recovered.head
    } else {
      run(program)(program(state.pc.toInt)(state))
    }
  }

  val program = input.map(parseInst).toIndexedSeq
  val result = run(program)(State(Map.empty, 0, Nil, Nil))

  println(result)
}


object Day18_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input18.txt").getLines.toSeq

  case class State(reg: Map[String, Long], pc: Long, sent: Queue[Long], receiving: Option[String], sendCount: Int) {
    def next: State = copy(pc = pc + 1)
    def updated(x: String, y: Long): State = copy(reg = reg.updated(x, y))
    def value(x: String): Long = reg.getOrElse(x,0)
    def consume: (Long, State) = {
      val (consumed, newQueue) = sent.dequeue
      (consumed, copy(sent = newQueue))
    }
    def receive(x: Long): State = updated(receiving.get, x).copy(receiving = None).next
    def isReceiving = receiving.nonEmpty
  }

  sealed trait Number { def value(state: State): Long }
  case class Literal(n: Long) extends Number {
    override def value(state: State): Long = n
  }
  case class Register(x: String) extends Number {
    override def value(state: State): Long = state.value(x)
  }

  sealed trait Inst { def apply(state: State): State }
  case class Snd(x: Number) extends Inst {
    def apply(state: State) = state.copy(sent = state.sent.enqueue(x.value(state)), sendCount = state.sendCount+1).next
  }
  case class Set(x: String, y: Number) extends Inst {
    override def apply(state: State) = state.updated(x, y.value(state)).next
  }
  case class Add(x: String, y: Number) extends Inst {
    override def apply(state: State) = state.updated(x,state.value(x) + y.value(state)).next
  }
  case class Mul(x: String, y: Number) extends Inst {
    override def apply(state: State) = state.updated(x,state.value(x) * y.value(state)).next
  }
  case class Mod(x: String, y: Number) extends Inst {
    override def apply(state: State) = state.updated(x,state.value(x) % y.value(state)).next
  }
  case class Rcv(x: String) extends Inst {
    override def apply(state: State) = {
      require(!state.isReceiving)
      state.copy(receiving = Some(x))
    }
  }
  case class Jgz(x: Number, y: Number) extends Inst {
    override def apply(state: State) = {
      if (x.value(state) > 0) {
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
      case Array("snd", x)    => Snd(parseNumber(x))
      case Array("set", x, y) => Set(x, parseNumber(y))
      case Array("add", x, y) => Add(x, parseNumber(y))
      case Array("mul", x, y) => Mul(x, parseNumber(y))
      case Array("mod", x, y) => Mod(x, parseNumber(y))
      case Array("rcv", x)    => Rcv(x)
      case Array("jgz", x, y) => Jgz(parseNumber(x), parseNumber(y))
    }
  }

  val program = input.map(parseInst).toIndexedSeq

  def hasTerminated(state: State) = !program.isDefinedAt(state.pc.toInt)

  def runUntilBlocked(state: State): State = {
    if (hasTerminated(state) || state.isReceiving) {
      state
    } else {
      runUntilBlocked(program(state.pc.toInt)(state))
    }
  }

  def run(p1: State, p2: State): Int = {
    val blocked1 = runUntilBlocked(p1)
    val blocked2 = runUntilBlocked(p2)

    if (blocked2.isReceiving && blocked1.sent.nonEmpty) {
      val (received, consumed1) = blocked1.consume
      run(consumed1, blocked2.receive(received))

    } else if (blocked1.isReceiving && blocked2.sent.nonEmpty) {
      val (received, consumed2) = blocked2.consume
      run(blocked1.receive(received), consumed2)

    } else {
      blocked2.sendCount
    }
  }

  val emptyState = State(Map.empty, 0, Queue.empty, None, 0)

  val result = run(emptyState, emptyState.updated("p", 1))

  println(result)
}
