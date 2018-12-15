package se.apogo.advent18

import scala.io.Source

object Day13 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input13.txt").getLines.toIndexedSeq

  case class Vec(x: Int, y: Int) {
    def +(v: Vec) = Vec(v.x + x, v.y + y)
    def ccw: Vec = Vec(y, -x)
    def cw: Vec = Vec(-y, x)
  }

  val Up = Vec(0, -1)
  val Down = Vec(0, 1)
  val Left = Vec(-1, 0)
  val Right = Vec(1, 0)

  case class Cart(id: Vec, pos: Vec, direction: Vec, turns: Seq[Vec => Vec]) {
    def move(state: State): Cart = {
      val (newDir, newTurns) = state.tracks(pos) match {
        case '-' | '|' => (direction, turns)
        case '\\' if Set(Up, Down).contains(direction)    => (direction.ccw, turns)
        case '\\' if Set(Left, Right).contains(direction) => (direction.cw,  turns)
        case '/'  if Set(Left, Right).contains(direction) => (direction.ccw, turns)
        case '/'  if Set(Up, Down).contains(direction)    => (direction.cw,  turns)
        case '+' => (turns.head(direction), turns.tail :+ turns.head)
      }
      copy(pos = pos + newDir, direction = newDir, turns = newTurns)
    }
  }

  case class State(tracks: Map[Vec,Char], carts: Set[Cart]) {
    def crash(): Option[(Vec, Set[Cart])] = carts.groupBy(_.pos).find(_._2.size > 1)

    def intersection(pos: Vec): Boolean = tracks(pos) == '+'
    def merge(state: State): State = State(tracks ++ state.tracks, carts ++ state.carts)
  }

  val turns: Seq[Vec => Vec] = Seq(_.ccw, v => v, _.cw)

  val init: State = {
    (for {
      y <- input.indices
      line = input(y)
      x <- line.indices
      c = line(x)
      pos = Vec(x,y)
    } yield {
      if ("/\\|-+".contains(c)) {
        State(Map(pos -> c), Set.empty)
      } else c match {
        case '^' => State(Map(pos -> '|'), Set(Cart(pos, pos, Vec(0 , -1), turns)))
        case 'v' => State(Map(pos -> '|'), Set(Cart(pos, pos, Vec(0 ,  1), turns)))
        case '<' => State(Map(pos -> '-'), Set(Cart(pos, pos, Vec(-1,  0), turns)))
        case '>' => State(Map(pos -> '-'), Set(Cart(pos, pos, Vec( 1,  0), turns)))
        case _   => State(Map.empty, Set.empty)
      }
    }).reduce(_ merge _)
  }

  def go(state: State): State = {
    val carts: Seq[Cart] = state.carts.toSeq.sortBy(_.pos.x).sortBy(_.pos.y)

    def moveCarts(cartState: State, toMove: Seq[Cart]): State = {
      if (toMove.isEmpty) cartState else {
        val cart = toMove.head
        val newState = cartState.copy(carts = (cartState.carts - cart) + cart.move(cartState))
        if (newState.crash().nonEmpty) newState else moveCarts(newState, toMove.tail)
      }
    }

    val newState = moveCarts(state, carts)
    if (newState.crash().nonEmpty) newState else go(newState)
  }

  val result = go(init)
  println(result.crash().get._1)
}

object Day13_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input13.txt").getLines.toIndexedSeq

  case class Vec(x: Int, y: Int) {
    def +(v: Vec) = Vec(v.x + x, v.y + y)
    def ccw: Vec = Vec(y, -x)
    def cw: Vec = Vec(-y, x)
  }

  val Up = Vec(0, -1)
  val Down = Vec(0, 1)
  val Left = Vec(-1, 0)
  val Right = Vec(1, 0)

  case class Cart(id: Vec, pos: Vec, direction: Vec, turns: Seq[Vec => Vec]) {
    def move(state: State): Cart = {
      val (newDir, newTurns) = state.tracks(pos) match {
        case '-' | '|' => (direction, turns)
        case '\\' if Set(Up, Down).contains(direction)    => (direction.ccw, turns)
        case '\\' if Set(Left, Right).contains(direction) => (direction.cw,  turns)
        case '/'  if Set(Left, Right).contains(direction) => (direction.ccw, turns)
        case '/'  if Set(Up, Down).contains(direction)    => (direction.cw,  turns)
        case '+' => (turns.head(direction), turns.tail :+ turns.head)
      }
      copy(pos = pos + newDir, direction = newDir, turns = newTurns)
    }
  }

  case class State(tracks: Map[Vec,Char], carts: Set[Cart]) {
    def crash(): Option[(Vec, Set[Cart])] = {
      val group = carts.groupBy(_.pos)
      group.find(_._2.size > 1)
    }

    def intersection(pos: Vec): Boolean = tracks(pos) == '+'
    def merge(state: State): State = State(tracks ++ state.tracks, carts ++ state.carts)
  }

  val turns: Seq[Vec => Vec] = Seq(_.ccw, v => v, _.cw)

  val init: State = {
    (for {
      y <- input.indices
      line = input(y)
      x <- line.indices
      c = line(x)
      pos = Vec(x,y)
    } yield {
      if ("/\\|-+".contains(c)) {
        State(Map(pos -> c), Set.empty)
      } else c match {
        case '^' => State(Map(pos -> '|'), Set(Cart(pos, pos, Vec(0 , -1), turns)))
        case 'v' => State(Map(pos -> '|'), Set(Cart(pos, pos, Vec(0 ,  1), turns)))
        case '<' => State(Map(pos -> '-'), Set(Cart(pos, pos, Vec(-1,  0), turns)))
        case '>' => State(Map(pos -> '-'), Set(Cart(pos, pos, Vec( 1,  0), turns)))
        case _   => State(Map.empty, Set.empty)
      }
    }).reduce(_ merge _)
  }

  def go(state: State): State = {
    val carts: Seq[Cart] = state.carts.toSeq.sortBy(_.pos.x).sortBy(_.pos.y)

    def moveCarts(cartState: State, toMove: Seq[Cart]): State = {
      if (toMove.isEmpty) cartState else {
        val cart = toMove.head
        val newState = cartState.copy(carts = (cartState.carts - cart) + cart.move(cartState))
        newState.crash() match {
          case None => moveCarts(newState, toMove.tail)
          case Some((_, crashCarts)) => {
            val removedState = newState.copy(carts = newState.carts -- crashCarts)
            moveCarts(removedState, toMove.tail)
          }
        }
      }
    }

    val newState = moveCarts(state, carts)
    if (newState.carts.size == 1) newState else go(newState)
  }

  println(go(init).carts.head.pos)
}
