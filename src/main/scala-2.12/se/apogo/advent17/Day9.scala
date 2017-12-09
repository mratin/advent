package se.apogo.advent17

import scala.io.Source

object Day9_1 extends App {
  val input: String = Source.fromResource("se/apogo/advent17/input09.txt").getLines.toSeq.head

  case class Group(groups: Seq[Group]) {
    def score: Int = scoreI(1)
    def scoreI(i: Int): Int = i + groups.map(_.scoreI(i+1)).sum
  }

  def skipGarbage(string: String): String = {
    string.head match {
      case '!' => skipGarbage(string.drop(2))
      case '>' => string.tail
      case _ => skipGarbage(string.tail)
    }
  }

  def parse(string: String, groups: Seq[Group]): (Group,String) = {
    if (string.isEmpty) {
      (groups.head, "")
    } else {
      string.head match {
        case '<' => parse(skipGarbage(string.tail), groups)
        case '{' => {
          val (group, rest) = parse(string.tail, Nil)
          parse(rest, groups :+ group)
        }
        case '}' => (Group(groups), string.tail)
        case ',' => parse(string.tail, groups)
      }
    }
  }

  val (group,rest) = parse(input, Nil)

  println(group.score)
}


object Day9_2 extends App {
  val input: String = Source.fromResource("se/apogo/advent17/input09.txt").getLines.toSeq.head

  case class Group(groups: Seq[Group]) {
    def score: Int = scoreI(1)
    def scoreI(i: Int): Int = i + groups.map(_.scoreI(i+1)).sum
  }

  def skipGarbage(string: String, i: Int): (String, Int) = {
    string.head match {
      case '!' => skipGarbage(string.drop(2), i)
      case '>' => (string.tail, i)
      case _ => skipGarbage(string.tail, i+1)
    }
  }

  def parse(string: String, groups: Seq[Group], garbage: Int): (Group, String, Int) = {
    if (string.isEmpty) {
      (groups.head, "", garbage)
    } else {
      string.head match {
        case '<' => {
          val (rest, i) = skipGarbage(string.tail, garbage)
          parse(rest, groups, i)
        }
        case '{' => {
          val (group, rest, i) = parse(string.tail, Nil, garbage)
          parse(rest, groups :+ group, i)
        }
        case '}' => (Group(groups), string.tail, garbage)
        case ',' => parse(string.tail, groups, garbage)
      }
    }
  }

  val (group,rest,garbage) = parse(input, Nil, 0)

  println(garbage)
}
