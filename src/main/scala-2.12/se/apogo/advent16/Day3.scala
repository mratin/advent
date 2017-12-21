package se.apogo.advent16

import scala.io.Source

object Day3_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent16/input3.txt").getLines.toSeq

  val result = input.count(line => {
    val sides = line.trim.split("\\s+").map(_.trim.toInt)
    Seq(Seq(0,1,2),Seq(1,2,0),Seq(0,2,1)).forall(s => sides(s(0)) + sides(s(1)) > sides(s(2)))
  })

  println(result)
}

object Day3_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent16/input3.txt").getLines.toSeq
  val numbers: Seq[Seq[Int]] = input.map(_.trim.split("\\s+").toSeq.map(_.trim.toInt))

  def isValid(sides: Seq[Int]): Boolean =
    Seq(Seq(0,1,2),Seq(1,2,0),Seq(0,2,1)).forall(s => sides(s(0)) + sides(s(1)) > sides(s(2)))

  val result = (for (i <- numbers.indices by 3; j <- 0 to 2) yield {
    for (k <- i to i+2) yield numbers(k)(j)
  }).count(isValid)

  println(result)
}
