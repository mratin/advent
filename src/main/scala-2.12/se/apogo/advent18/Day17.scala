package se.apogo.advent18

import scala.io.Source

object Day17 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input17.txt").getLines.toSeq

  case class Vec(x: Int, y: Int) {
    def down = copy(y = y+1)
    def left = copy(x = x-1)
    def right = copy(x = x+1)
  }

  case class State(water: Set[Vec], clay: Set[Vec], outs: Set[Vec]) {
    def isEmpty(v: Vec) = !clay.contains(v) && !water.contains(v)

    def expand(d: Vec => Vec, from: Vec): State =
      if (!water.contains(from)) this else addOut(from).expand(d, d(from))

    def add(w: Vec) = copy(water = water + w)
    def addOut(w: Vec): State = copy(outs = outs + w)
  }

  def parse(line: String): Set[Vec] = {
    val Array(l,r) = line.split(",").map(_.trim)
    val Array(d,v) = l.split("=")
    val Array(_,range) = r.split("=")
    val Array(from,end) = range.split("\\.\\.").map(_.toInt)
    val clay = d match {
      case "x" => for (y <- from to end) yield Vec(v.toInt,y)
      case "y" => for (x <- from to end) yield Vec(x, v.toInt)
    }
    clay.toSet
  }

  val init: State = State(water = Set.empty, clay = input.flatMap(parse).toSet, Set.empty)
  val minY = init.clay.map(_.y).min
  val maxY = init.clay.map(_.y).max

  def run(state: State, w: Vec): (State, Boolean) = {
    if (state.outs.contains(w) || w.y >= maxY) {
      (state.add(w).addOut(w), true)
    } else if (state.clay.contains(w.down)) {
      spreadSides(state.add(w), w)
    } else {
      val (downState, out) = run(state.add(w), w.down)
      if (out) {
        (downState.addOut(w), true)
      } else {
        spreadSides(downState, w)
      }
    }
  }

  def spreadSides(state: State, w: Vec): (State, Boolean) = {
    def spreadSide(s: State, next: Vec): (State, Boolean) =
      Option(next).filter(s.isEmpty).map(v => run(s.add(w), v)).getOrElse((s, s.outs.contains(next)))

    val (s,o) = spreadSide(state, w.left)
    val (sr, or) = spreadSide(s, w.right)

    if (o || or) {
      (sr.expand(_.right, w).expand(_.left, w), true)
    } else {
      (sr, false)
    }
  }

  def withinBounds(v: Vec) = minY <= v.y && v.y <= maxY
  val (result,_) = run(init, Vec(500, 0))
  val within = result.water.count(withinBounds)
  println("Part1: " + within)
  println("Part2: " + (within - result.outs.size))
}
