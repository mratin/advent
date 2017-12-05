package se.apogo.advent17

import scala.io.Source

object Day1 extends App {
  val inputString = Source.fromResource("se/apogo/advent17/input01.txt").getLines.mkString("\n").trim

  val inputSeq = inputString.map(_.toString).map(Integer.parseInt)

  val circular = inputSeq :+ inputSeq.head

  val result = circular.zip(circular.tail).filter({case (a,b) => a == b}).map(_._1).sum

  println(result)
}

object Day1_2 extends App {
  val inputString = Source.fromResource("se/apogo/advent17/input01.txt").getLines.mkString("\n").trim

  val inputSeq = inputString.map(_.toString).map(Integer.parseInt)

  val (beginning, end) = inputSeq.splitAt(inputSeq.size / 2)

  val shifted = end ++ beginning

  val result = inputSeq.zip(shifted).filter({case (a,b) => a == b}).map(_._1).sum

  println(result)
}
