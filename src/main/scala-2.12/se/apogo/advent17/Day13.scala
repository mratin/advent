package se.apogo.advent17

import scala.io.Source

object Day13_1 extends App {
  val input: List[String] = Source.fromResource("se/apogo/advent17/input13.txt").getLines.toList

  case class Layer(depth: Int, range: Int) {
    def severity(t: Int) = if (t % (range * 2 - 2) == 0) depth * range else 0
  }

  def parseLine(line: String): Layer = {
    val Array(depth, range) = line.split(":").map(_.trim).map(Integer.parseInt)
    Layer(depth, range)
  }

  val layers = input.map(parseLine).map(l => l.depth -> l).toMap

  val result = (0 to layers.keys.max).flatMap(t => layers.get(t).map(_.severity(t))).sum

  println(result)
}


object Day13_2 extends App {
  val input: List[String] = Source.fromResource("se/apogo/advent17/input13.txt").getLines.toList

  case class Layer(depth: Int, range: Int) {
    def isSafe(t: Int): Boolean = t % (range * 2 - 2) != 0
  }

  def parseLine(line: String): Layer = {
    val Array(depth, range) = line.split(":").map(_.trim).map(Integer.parseInt)
    Layer(depth, range)
  }

  val layers = input.map(parseLine).map(l => l.depth -> l).toMap
  val maxDepth = layers.keys.max

  def isSafe(delay: Int): Boolean = {
    (delay to maxDepth + delay).zipWithIndex.
      forall({case (t, depth) => layers.get(depth).forall(l => l.isSafe(t))})
  }

  val result = Stream.from(0).find(isSafe)

  println(result)
}
