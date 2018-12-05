package se.apogo.advent18

import scala.io.Source

object Day05 extends App {
  val input: String = Source.fromResource("se/apogo/advent18/input05.txt").getLines.mkString.trim

  def react(string: List[Char], acc: List[Char]): List[Char] = string match {
    case a::b::tail if a != b && (a.toUpper == b || a == b.toUpper) => react(tail, acc)
    case a::tail => react(tail, a::acc)
    case Nil => acc
  }

  def reactAll(string: List[Char]): List[Char] = {
    val r = react(string, Nil)
    if (r.lengthCompare(string.size) != 0) reactAll(r) else string
  }

  println(reactAll(input.toList).size)
}

object Day05_2 extends App {
  val input: List[Char] = Source.fromResource("se/apogo/advent18/input05.txt").getLines.mkString.toList

  def react(string: List[Char], acc: List[Char]): List[Char] = string match {
    case a::b::tail if a != b && (a.toUpper == b || a == b.toUpper) => react(tail, acc)
    case a::tail => react(tail, a::acc)
    case Nil => acc
  }

  def reactAll(string: List[Char]): List[Char] = {
    val r = react(string, Nil)
    if (r.lengthCompare(string.size) != 0) reactAll(r) else string
  }

  val shortest = input.map(_.toUpper).toSet
    .map((u: Char) => reactAll(input.filter(_.toUpper != u)).size).min

  println(shortest)
}
