package se.apogo.advent18

import scala.io.Source

object Day03 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input03.txt").getLines.map(_.trim).toSeq

  case class Rect(id: Int, x: Int, y: Int, w: Int, h: Int)

  def parseLine(line: String): Rect = {
    val Array(idk, rest) = line.split("@")
    val id = Integer.parseInt(idk.tail.trim)
    val Array(posk, sizek) = rest.split(":")
    val Array(x,y) = posk.split(",").map(_.trim).map(Integer.parseInt)
    val Array(w,h) = sizek.split("x").map(_.trim).map(Integer.parseInt)
    Rect(id,x,y,w,h)
  }

  def add(board: Map[(Int,Int), Int], rect: Rect): Map[(Int,Int), Int] = {
    val positions = for (i <- rect.x until rect.x + rect.w; j <- rect.y until rect.y + rect.h) yield (i,j)

    positions.foldLeft(board)((b, p) => b.updated(p, b.getOrElse(p, 0) + 1))
  }

  val board = input.map(parseLine).foldLeft(Map[(Int,Int), Int]())(add)

  println(board.values.count(_ > 1))
}

object Day03_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input03.txt").getLines.map(_.trim).toSeq

  case class Rect(id: Int, x: Int, y: Int, w: Int, h: Int) {
    val positions: Seq[(Int, Int)] = for (i <- x until x + w; j <- y until y + h) yield (i,j)
  }

  def parseLine(line: String): Rect = {
    val Array(idk, rest) = line.split("@")
    val id = Integer.parseInt(idk.tail.trim)
    val Array(posk, sizek) = rest.split(":")
    val Array(x,y) = posk.split(",").map(_.trim).map(Integer.parseInt)
    val Array(w,h) = sizek.split("x").map(_.trim).map(Integer.parseInt)
    Rect(id,x,y,w,h)
  }

  def add(board: Map[(Int,Int), Set[Int]], rect: Rect): Map[(Int,Int), Set[Int]] =
    rect.positions.foldLeft(board)((b, p) => b.updated(p, b.getOrElse(p, Set[Int]()) + rect.id))

  val rects = input.map(parseLine)
  val board = rects.foldLeft(Map[(Int,Int), Set[Int]]())(add)

  def noOverlaps(rect: Rect): Boolean = rect.positions.map(board).forall(_.size == 1)

  println(rects.find(noOverlaps))
}
