package se.apogo.advent18

import scala.io.Source

object Day18 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input18.txt").getLines.toIndexedSeq

  case class Vec(x: Int, y: Int) {
    lazy val adjacent: Seq[Vec] = for (i <- -1 to 1; j <- -1 to 1; if !(i==0 && j==0)) yield Vec(x+i,y+j)
  }

  case class State(board: Map[Vec, Char]) {
    lazy val evolve: State = {
      State((for {
        x <- (0 to xMax).par
        y <- (0 to yMax).par
        pos = Vec(x, y)
        adjacent = pos.adjacent.filter(onBoard).map(board)
      } yield {
        pos -> (board(pos) match {
          case '.' if adjacent.count(_ == '|') >= 3 => '|'
          case '|' if adjacent.count(_ == '#') >= 3 => '#'
          case '#' => if (adjacent.count(_ == '#') >= 1 && adjacent.count(_ == '|') >= 1) '#' else '.'
          case c => c
        })
      }).toMap.seq)
    }

    lazy val value: Int = board.values.count(_ == '|') * board.values.count(_ == '#')
  }

  val init: State = State((for (y <- input.indices; line = input(y); x <- line.indices) yield Vec(x,y) -> line(x)).toMap)
  val xMax = init.board.keys.map(_.x).max
  val yMax = init.board.keys.map(_.y).max
  val end = 10

  def onBoard(v: Vec) = 0 <= v.x && v.x <= xMax && 0 <= v.y && v.y <= xMax

  def tick(state: State, i: Int): State = if (i == end) state else tick(state.evolve, i+1)

  println(tick(init, 0).value)
}


object Day18_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input18.txt").getLines.toIndexedSeq

  case class Vec(x: Int, y: Int) {
    lazy val adjacent: Seq[Vec] = for (i <- -1 to 1; j <- -1 to 1; if !(i == 0 && j == 0)) yield Vec(x + i, y + j)
  }

  case class State(board: Map[Vec, Char]) {
    lazy val evolve: State = {
      State((for {
        x <- (0 to xMax).par
        y <- (0 to yMax).par
        pos = Vec(x, y)
        adjacent = pos.adjacent.filter(onBoard).map(board)
      } yield {
        pos -> (board(pos) match {
          case '.' if adjacent.count(_ == '|') >= 3 => '|'
          case '|' if adjacent.count(_ == '#') >= 3 => '#'
          case '#' => if (adjacent.count(_ == '#') >= 1 && adjacent.count(_ == '|') >= 1) '#' else '.'
          case c => c
        })
      }).toMap.seq)
    }

    lazy val value: Int = board.values.count(_ == '|') * board.values.count(_ == '#')
  }

  val init: State = State((for (y <- input.indices; line = input(y); x <- line.indices) yield Vec(x,y) -> line(x)).toMap)
  val xMax = init.board.keys.map(_.x).max
  val yMax = init.board.keys.map(_.y).max

  val end = 1000000000

  def onBoard(v: Vec) = 0 <= v.x && v.x <= xMax && 0 <= v.y && v.y <= xMax

  def findCycle(state: State, i: Int, history: Map[State, Int]): (State, Int, Int) =
    if (history.contains(state.evolve)) {
      (state.evolve, i - history(state.evolve), i)
    } else {
      findCycle(state.evolve, i+1, history.updated(state.evolve, i))
    }

  val (cycleStart, cycleSize, offset) = findCycle(init, 0, Map())

  def tick(state: State, i: Int): State = if (i % cycleSize == 0) state else tick(state.evolve, i-1)

  println(tick(cycleStart, end-offset-1).value)
}
