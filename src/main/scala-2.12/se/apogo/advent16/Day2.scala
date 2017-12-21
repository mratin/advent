package se.apogo.advent16

import scala.io.Source

object Day2_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent16/input2.txt").getLines.toSeq

  case class Vec(x:Int, y:Int) {
    def +(v: Vec) = Vec(x+v.x, y+v.y)
    def clamp = Vec(math.min(math.max(0, x),2), math.min(math.max(0, y),2))
  }

  def go(from: Vec, c: Char): Vec = {
    val move = c match {
      case 'U' => Vec(0,-1)
      case 'D' => Vec(0, 1)
      case 'R' => Vec(1, 0)
      case 'L' => Vec(-1,0)
    }
    (from + move).clamp
  }

  val keypad = Seq(1 to 3, 4 to 6, 7 to 9)
  def numbers(from: Vec, lines: Seq[String], ns: Seq[Int]): Seq[Int] = {
    lines.headOption match {
      case None => ns
      case Some(line) =>
        val next = line.foldLeft(from)(go)
        numbers(next, lines.tail, ns :+ keypad(next.y)(next.x))
    }
  }

  val result = numbers(Vec(1,1), input, Nil)
  println(result.mkString)
}


object Day2_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent16/input2.txt").getLines.toSeq

  val keypad = Map(
    Vec(2,0) -> '1',
    Vec(1,1) -> '2', Vec(2,1) -> '3', Vec(3,1) -> '4',
    Vec(0,2) -> '5', Vec(1,2) -> '6', Vec(2,2) -> '7', Vec(3,2) -> '8', Vec(4,2) -> '9',
    Vec(1,3) -> 'A', Vec(2,3) -> 'B', Vec(3,3) -> 'C',
    Vec(2,4) -> 'D')

  case class Vec(x:Int, y:Int) {
    def +(v: Vec) = Vec(x+v.x, y+v.y)
  }

  def go(from: Vec, c: Char): Vec = {
    val move = c match {
      case 'U' => Vec(0,-1)
      case 'D' => Vec(0, 1)
      case 'R' => Vec(1, 0)
      case 'L' => Vec(-1,0)
    }
    val next = from + move
    if (keypad.isDefinedAt(next)) next else from
  }

  def numbers(from: Vec, lines: Seq[String], ns: Seq[Char]): Seq[Char] = {
    lines.headOption match {
      case None => ns
      case Some(line) =>
        val next = line.foldLeft(from)(go)
        numbers(next, lines.tail, ns :+ keypad(next))
    }
  }

  val result = numbers(Vec(0,2), input, Nil)
  println(result.mkString)
}
