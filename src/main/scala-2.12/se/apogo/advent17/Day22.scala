package se.apogo.advent17

import scala.io.Source

object Day22_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input22.txt").getLines.toSeq

  case class Vec(x:Int, y:Int) {
    def +(v: Vec) = Vec(x+v.x, y+v.y)
    def left = Vec(-y,x)
    def right = Vec(y,-x)
  }

  type Grid = Set[Vec]

  def parseGrid(lines: Seq[String]): Grid = {
    (for {
      y <- lines.indices
      line = lines(y)
      x <- line.indices
      if line(x) == '#'
    } yield {
      Vec(x, -y)
    }).toSet
  }

  def go(from: Vec, facing: Vec, grid: Grid, bursts: Int, infecting: Int): Int = {
    if (bursts == 0) {
      infecting
    } else {
      if (grid.contains(from)) {
        go(from + facing.right, facing.right, grid - from, bursts-1, infecting)
      } else {
        go(from + facing.left, facing.left, grid + from, bursts-1, infecting + 1)
      }
    }
  }

  val start = Vec(input.head.length / 2  , -input.size / 2 )
  val result = go(start, Vec(0,1), parseGrid(input), 10000, 0)

  println(result)
}

object Day22_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input22.txt").getLines.toSeq

  case class Vec(x:Int, y:Int) {
    def +(v: Vec) = Vec(x+v.x, y+v.y)
    def left = Vec(-y,x)
    def right = Vec(y,-x)
    def reverse = Vec(-x,-y)
  }

  case class Grid(infected: Set[Vec], weakened: Set[Vec], flagged: Set[Vec])

  def parseGrid(lines: Seq[String]): Grid = {
    val infected = (for {
      y <- lines.indices
      line = lines(y)
      x <- line.indices
      if line(x) == '#'
    } yield {
      Vec(x, -y)
    }).toSet

    Grid(infected, Set.empty, Set.empty)
  }

  def go(from: Vec, facing: Vec, grid: Grid, bursts: Int, infecting: Int): Int = {
    if (bursts == 0) {
      infecting
    } else {
      if (grid.weakened.contains(from)) {
        go(from + facing, facing,
          grid.copy(infected = grid.infected + from, weakened = grid.weakened - from),
          bursts-1, infecting + 1)
      } else if (grid.infected.contains(from)) {
        go(from + facing.right, facing.right,
          grid.copy(infected = grid.infected - from, flagged = grid.flagged + from),
          bursts-1, infecting)
      } else if (grid.flagged.contains(from)) {
        go(from + facing.reverse, facing.reverse,
          grid.copy(flagged = grid.flagged - from), bursts-1, infecting)
      } else {
        go(from + facing.left, facing.left,
          grid.copy(weakened = grid.weakened + from), bursts-1, infecting)
      }
    }
  }

  val start = Vec(input.head.length / 2  , -input.size / 2 )
  val result = go(start, Vec(0,1), parseGrid(input), 10000000, 0)

  println(result)
}
