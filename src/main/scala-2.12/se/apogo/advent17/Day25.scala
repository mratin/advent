package se.apogo.advent17

import scala.io.Source

object Day25 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input25.txt").getLines.toSeq

  case class Tape(setPositions: Set[Int], cursor: Int) {
    def currentValue: Boolean = setPositions.contains(cursor)
    def write(v: Boolean): Tape = if (v) copy(setPositions = setPositions + cursor) else copy(setPositions = setPositions - cursor)
    def move(r: Boolean) = if (r) copy(cursor = cursor+1) else copy(cursor = cursor-1)
  }

  case class Action(write: Boolean, right: Boolean, next: String)
  case class State(name: String, actionTrue: Action, actionFalse: Action)

  def parseStates(lines: Seq[String]): Seq[State] = {
    if (lines.isEmpty) Nil else {

      def parseAction(actionLines: Seq[String]) = {
        val write = actionLines(0).split("\\s").last.init.toInt
        val right = actionLines(1).split("\\s").last.init == "right"
        val next = actionLines(2).split("\\s").last.init
        Action(write == 1, right, next)
      }

      val name = lines.head.split("\\s").last.init
      val actionFalse = parseAction(lines.slice(2, 5))
      val actionTrue = parseAction(lines.slice(6, 9))

      parseStates(lines.drop(9)) :+ State(name, actionTrue, actionFalse)
    }
  }

  val stateMap: Map[String, State] = parseStates(input.filter(_.trim.nonEmpty).drop(2)).map(state => state.name -> state).toMap

  def run(tape: Tape, state: State, i: Int): Tape = {
    if (i == 0) tape else {
      val action = if (tape.currentValue) state.actionTrue else state.actionFalse
      val newTape = tape.write(action.write).move(action.right)
      val next = stateMap(action.next)
      run(newTape, next, i-1)
    }
  }

  val steps = input(1).split("\\s")(5).toInt
  val result = run(Tape(Set.empty, 0), stateMap("A"), steps)

  println(result.setPositions.size)
}

