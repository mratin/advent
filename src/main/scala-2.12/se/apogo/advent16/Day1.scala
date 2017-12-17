package se.apogo.advent16

import scala.io.Source

object Day1_1 extends App {
  val input: List[String] = Source.fromResource("se/apogo/advent16/input1.txt").
    getLines.toSeq.head.split(",").map(_.trim).toList

  case class Vec(x:Int, y:Int) {
    def +(v: Vec) = Vec(x+v.x, y+v.y)
    def left = Vec(-y,x)
    def right = Vec(y,-x)
    def scale(n: Int) = Vec(x*n, y*n)
  }

  case class Position(loc: Vec, facing: Vec)
  case class Move(turn: Char, steps: Int)

  def go(position: Position, move: Move): Position = {
    val facing = move.turn match {
      case 'L' => position.facing.left
      case 'R' => position.facing.right
    }
    Position(position.loc + facing.scale(move.steps), facing)
  }

  val moves = input.map(s => Move(s.head, Integer.parseInt(s.tail)))
  val result = moves.foldLeft(Position(Vec(0,0), Vec(0,1)))(go)

  println(math.abs(result.loc.x) + math.abs(result.loc.y))
}

object Day1_2 extends App {
  val input: List[String] = Source.fromResource("se/apogo/advent16/input1.txt").
    getLines.toSeq.head.split(",").map(_.trim).toList

  case class Vec(x:Int, y:Int) {
    def +(v: Vec) = Vec(x+v.x, y+v.y)
    def left = Vec(-y,x)
    def right = Vec(y,-x)
  }

  sealed trait Move
  case object Forward extends Move
  case object Left extends Move
  case object Right extends Move

  def go(position: Vec, facing: Vec, moves: Seq[Move], visited: Set[Vec]): Vec = {
    if (visited.contains(position)) {
      position
    } else {
      moves.head match {
        case Left    => go(position, facing.left, moves.tail, visited)
        case Right   => go(position, facing.right, moves.tail, visited)
        case Forward => go(position + facing, facing, moves.tail, visited + position)
      }
    }
  }

  def parseMove(string: String): Seq[Move] = {
    val turn = string.head match {
      case 'R' => Right
      case 'L' => Left
    }
    turn :: List.fill(Integer.parseInt(string.tail))(Forward)
  }

  val moves = input.flatMap(parseMove)
  val result = go(Vec(0,0), Vec(0,1), moves, Set.empty)

  println(math.abs(result.x) + math.abs(result.y))
}
