package se.apogo.advent18

import scala.io.Source

object Day07 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input07.txt").getLines.toSeq

  def parse(line: String): (Char, Char) = (line(5), line(36))

  val dependencies: Map[Char, Seq[Char]] = input.map(parse).groupBy(_._2).mapValues(_.map(_._1))
  val nodes = dependencies.keySet ++ dependencies.values.flatten.toSet

  def available(unvisited: Seq[Char])(c: Char) = dependencies.getOrElse(c, Set.empty).forall(!unvisited.contains(_))

  def go(steps: Seq[Char], unvisited: Seq[Char]): Seq[Char] = {
    unvisited.find(available(unvisited)) match {
      case Some(next) => go(steps :+ next, unvisited.filterNot(_ == next))
      case None => steps
    }}

  println(go(Nil, nodes.toSeq.sorted).mkString)
}

object Day07_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input07.txt").getLines.toSeq

  def parse(line: String): (Char, Char) = (line(5), line(36))

  val dependencies: Map[Char, Seq[Char]] = input.map(parse).groupBy(_._2).mapValues(_.map(_._1))
  val nodes = dependencies.keySet ++ dependencies.values.flatten.toSet

  def available(notComplete: Set[Char])(c: Char) = dependencies.getOrElse(c, Set.empty).forall(!notComplete.contains(_))

  case class Work(c: Char, t: Int) {
    def tick = copy(t = t+1)
    def done = 61 + c.toInt - 'A'.toInt == t
  }

  def tick(time: Int, ongoing: Set[Work], unvisited: Seq[Char]): Int = {
    if (ongoing.isEmpty && unvisited.isEmpty) {
      time
    } else {
      val notComplete = unvisited ++ ongoing.map(_.c)
      val nexts = unvisited.filter(available(notComplete.toSet)).take(5 - ongoing.size)
      val nextWork = nexts.map(Work(_, 0))

      if (ongoing.size < 5 && nextWork.nonEmpty) {
        tick(time, ongoing ++ nextWork.toSet, unvisited.filterNot(nexts.contains))
      } else {
        tick(time+1, ongoing.map(_.tick).filterNot(_.done), unvisited)
      }}
  }

  println(tick(0, Set.empty, nodes.toSeq.sorted))
}
