package se.apogo.advent17

import scala.io.Source

object Day16_1 extends App {
  val input: List[String] = Source.fromResource("se/apogo/advent17/input16.txt").
    getLines.toSeq.head.split(",").map(_.trim).toList

  sealed trait Move
  case class S(x: Int) extends Move
  case class X(a: Int, b: Int) extends Move
  case class P(a: Char, b: Char) extends Move

  def parseMove(string: String): Move = {
    string.head match {
      case 's' => S(Integer.parseInt(string.tail))
      case 'x' =>
        val Array(a,b) = string.tail.split("/").map(Integer.parseInt)
        X(a,b)
      case 'p' =>
        val Array(a,b) = string.tail.split("/")
        P(a.head,b.head)
    }
  }

  def dance(line: IndexedSeq[Char], move: Move): IndexedSeq[Char] = {
    move match {
      case S(x)   => line.takeRight(x) ++ line.dropRight(x)
      case X(a,b) => line.updated(a, line(b)).updated(b, line(a))
      case P(a,b) =>
        val ai = line.indexWhere(_ == a)
        val bi = line.indexWhere(_ == b)
        line.updated(ai, line(bi)).updated(bi, line(ai))
    }
  }

  val init: scala.collection.IndexedSeq[Char] = "abcdefghijklmnop".toIndexedSeq
  val result = input.map(parseMove).foldLeft(init)(dance)

  println(result.mkString)
}


object Day16_2 extends App {
  val input: List[String] = Source.fromResource("se/apogo/advent17/input16.txt").
    getLines.toSeq.head.split(",").map(_.trim).toList

  sealed trait Move
  case class S(x: Int) extends Move
  case class X(a: Int, b: Int) extends Move
  case class P(a: Char, b: Char) extends Move

  def parseMove(string: String): Move = {
    string.head match {
      case 's' => S(Integer.parseInt(string.tail))
      case 'x' =>
        val Array(a,b) = string.tail.split("/").map(Integer.parseInt)
        X(a,b)
      case 'p' =>
        val Array(a,b) = string.tail.split("/")
        P(a.head,b.head)
    }
  }

  def dance(line: IndexedSeq[Char], move: Move): IndexedSeq[Char] = {
    move match {
      case S(x)   => line.takeRight(x) ++ line.dropRight(x)
      case X(a,b) => line.updated(a, line(b)).updated(b, line(a))
      case P(a,b) =>
        val ai = line.indexWhere(_ == a)
        val bi = line.indexWhere(_ == b)
        line.updated(ai, line(bi)).updated(bi, line(ai))
    }
  }

  val init: scala.collection.IndexedSeq[Char] = "abcdefghijklmnop".toIndexedSeq
  val moves = input.map(parseMove)

  def cycle(line: IndexedSeq[Char], lines: Seq[IndexedSeq[Char]]): Seq[IndexedSeq[Char]] = {
    val next = moves.foldLeft(line)(dance)
    if (lines.headOption.contains(next)) {
      lines
    } else {
      cycle(next, lines :+ next)
    }
  }

  val cyc = cycle(init, Seq(init))
  val result = cyc(1000000000 % cyc.size)

  println(result.mkString)
}
