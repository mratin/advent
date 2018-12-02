package se.apogo.advent18

import scala.io.Source

object Day02 extends App {
  val changes: Seq[String] = Source.fromResource("se/apogo/advent18/input02.txt").getLines.map(_.trim).toSeq

  def count(s: String)(c: Char) = s.count(_ == c)
  val counts: Seq[IndexedSeq[Int]] = changes.map(s => s.map(count(s)))

  println(counts.count(_.exists(_ == 2)) * counts.count(_.exists(_ == 3)))
}

object Day02_2 extends App {
  val changes: Seq[List[Char]] = Source.fromResource("se/apogo/advent18/input02.txt").getLines.map(_.trim.toList).toSeq

  def check(l1: List[Char], l2: List[Char], acc: String, change: Boolean): Option[String] = {
    (l1, l2) match {
      case (Nil, _) if change => Some(acc)
      case (Nil, _) => None
      case (a::as, b::bs) if !change && a != b => check(as, bs, acc, true)
      case (a::_, b::_) if a != b => None
      case (a::as, _::bs) => check(as, bs, acc :+ a, change)
    }
  }

  val result =
    for {
      i <- 0 to changes.size - 2
      j <- i + 1 until changes.size
      letters <- check(changes(i), changes(j), "", false)
    } yield letters

  println(result.head)
}


/**
  * More streamlined solution to part 2
  */
object Day02_2b extends App {
  val changes: Seq[String] = Source.fromResource("se/apogo/advent18/input02.txt").getLines.map(_.trim).toSeq

  val result =
    for {
      i <- 0 to changes.size - 2
      j <- i + 1 until changes.size
      letters = changes(i).zip(changes(j)).filter(p => p._1 == p._2).map(_._1).mkString
      if letters.length == changes(i).length - 1
    } yield letters

  println(result.head)
}
