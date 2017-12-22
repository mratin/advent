package se.apogo.advent16

import scala.io.Source

object Day7_1 extends App {
  val input = Source.fromResource("se/apogo/advent16/input7.txt").getLines.toSeq

  def isAbba(string: String) =
    string(0) != string(1) && string(0) == string(3) && string(1) == string(2)

  def hasAbba(string: String) = string.sliding(4).exists(isAbba)

  def isValid(string: String): Boolean = {
    val noAbba = """\[[^\]]*\]""".r.findAllIn(string)

    noAbba.forall(!hasAbba(_)) && hasAbba(string)
  }

  val result = input.count(isValid)

  println(result)
}

object Day7_2 extends App {
  val input = Source.fromResource("se/apogo/advent16/input7.txt").getLines.toSeq

  def isAba(string: String) = string(0) != string(1) && string(0) == string(2)

  def abas(string: String): Seq[String] = string.sliding(3).filter(isAba).toSeq

  def hasAba(aba: String)(string: String) = string.sliding(3).contains(aba)

  def invert(aba: String): String = Seq(aba(1),aba(0),aba(1)).mkString

  def isValid(string: String): Boolean = {
    val bracketPattern = """\[[^\]]*\]"""
    val betweenBrackets = bracketPattern.r.findAllIn(string).toList
    val outsideBrackets = string.split(bracketPattern).toList

    outsideBrackets.flatMap(abas).exists(aba => betweenBrackets.exists(hasAba(invert(aba))))
  }

  val result = input.count(isValid)

  println(result)
}
