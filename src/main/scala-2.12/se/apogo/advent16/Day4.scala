package se.apogo.advent16

import scala.io.Source

object Day4_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent16/input4.txt").getLines.toSeq

  def isValid(line: String): Boolean = {
    val checksum = line.trim.takeRight(6).init
    val name = line.takeWhile(!_.isDigit)

    val check = name.filter(_.isLetter).distinct.map(c => c -> name.count(_ == c)).
      sortBy(_._1).sortBy(-_._2).take(5).map(_._1).mkString

    check == checksum
  }

  def id(room: String): Int = room.filter(_.isDigit).toInt

  val result = input.map(_.trim).filter(isValid).map(id).sum

  println(result)
}

object Day4_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent16/input4.txt").getLines.toSeq

  case class Room(name: String, id: Int)

  def decrypt(room: String): Room = {
    val roomId = room.filter(_.isDigit).toInt
    val alphabet = "abcdefghijklmnopqrstuvwxyz"
    val key = alphabet.zipWithIndex.map({case (c,i) => c -> alphabet((i+roomId) % alphabet.length)}).toMap + ('-' -> ' ')
    val name = room.takeWhile(!_.isDigit)
    Room(name.map(key), roomId)
  }

  val result = input.map(decrypt).find(_.name.contains("northpole")).get.id

  println(result)
}

