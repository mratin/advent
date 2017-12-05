package se.apogo.advent17

object Day3 extends App {
  case class Position(x:Int,y:Int) {
    def move(direction: Direction) = {
      direction match {
        case Up => Position(x,y-1)
        case Down => Position(x,y+1)
        case Left => Position(x-1,y)
        case Right => Position(x+1,y)
      }
    }
  }

  sealed trait Direction {
    def ccw: Direction
  }
  case object Right extends Direction { def ccw = Up }
  case object Up extends Direction { def ccw = Left }
  case object Left extends Direction { def ccw = Down }
  case object Down extends Direction { def ccw = Right }

  def fill(n: Int, upTo: Int, position: Position, direction: Direction, filled: Set[Position]): Position = {
    if (n == upTo) position else {
      val turned = position.move(direction.ccw)
      val forward = position.move(direction)

      if (filled.contains(turned)) {
        fill(n+1, upTo, forward, direction, filled + forward)
      } else {
        fill(n+1, upTo, turned, direction.ccw, filled + turned)
      }
    }
  }

  val input = 265149
//  val input = 5

  val result = fill(2, upTo = input, Position(1,0), Right, Set(Position(0,0), Position(1,0)))

  println(result)
}


object Day3_2 extends App {

  case class Position(x:Int,y:Int) {
    def move(direction: Direction) = {
      direction match {
        case Up => Position(x,y-1)
        case Down => Position(x,y+1)
        case Left => Position(x-1,y)
        case Right => Position(x+1,y)
      }
    }

    def neighbours: Set[Position] = {
      Set(
        Position(x-1,y-1), Position(x,y-1), Position(x+1,y-1),
        Position(x-1,y), Position(x+1,y),
        Position(x-1,y+1), Position(x,y+1), Position(x+1,y+1))
    }
  }

  sealed trait Direction {
    def ccw: Direction
  }
  case object Right extends Direction { def ccw = Up }
  case object Up extends Direction { def ccw = Left }
  case object Left extends Direction { def ccw = Down }
  case object Down extends Direction { def ccw = Right }

  def fill(n: Int, upTo: Int, position: Position, direction: Direction, filled: Map[Position, Int]): Int = {
    if (n > upTo) n else {
      val turned = position.move(direction.ccw)
      val forward = position.move(direction)

      def value(pos: Position) = {
        val ns = pos.neighbours.toSeq.map(p => filled.getOrElse(p, 0))
        ns.sum
      }

      if (filled.contains(turned)) {
        val v = value(forward)

        fill(v, upTo, forward, direction, filled.updated(forward, value(forward)))
      } else {
        val v = value(turned)

        fill(v, upTo, turned, direction.ccw, filled.updated(turned, value(turned)))
      }
    }
  }

  val input = 265149
//    val input = 5

  val result = fill(1, upTo = input, Position(1,0), Right, Map(Position(0,0) -> 1, Position(1,0) -> 1))

  println(result)
}
