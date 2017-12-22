package se.apogo.advent16

import scala.io.Source

object Day8 extends App {
  val input = Source.fromResource("se/apogo/advent16/input8.txt").getLines.toSeq
  val sizeX = 50
  val sizeY = 6

  case class Pixel(x:Int, y:Int) {
    def shiftX(by: Int) = copy(x = (x + by) % sizeX)
    def shiftY(by: Int) = copy(y = (y + by) % sizeY)
  }

  type Grid = Set[Pixel]

  sealed trait Inst { def apply(grid: Grid): Grid }
  case class Rect(a: Int, b: Int) extends Inst {
    override def apply(grid: Grid): Grid = {
      val pixels = for (x <- 0 until a; y <- 0 until b) yield Pixel(x,y)
      grid ++ pixels
    }
  }

  case class RotateRow(y: Int, by: Int) extends Inst {
    override def apply(grid: Grid): Grid = {
      val pixels = grid.filter(_.y == y)
      (grid -- pixels) ++ pixels.map(_.shiftX(by))
    }
  }

  case class RotateCol(x: Int, by: Int) extends Inst {
    override def apply(grid: Grid): Grid = {
      val pixels = grid.filter(_.x == x)
      (grid -- pixels) ++ pixels.map(_.shiftY(by))
    }
  }

  def parseInst(line: String): Inst = {
    line.split("\\s") match {
      case Array("rect", rect) =>
        val Array(a,b) = rect.split("x")
        Rect(a.toInt, b.toInt)

      case Array("rotate", "row", yeq, "by", by) =>
        RotateRow(yeq.substring(2).toInt, by.toInt)

      case Array("rotate", "column", xeq, "by", by) =>
        RotateCol(xeq.substring(2).toInt, by.toInt)
    }
  }

  def printGrid(grid: Grid): Unit = {
    for (j <- 0 until sizeY) {
      for (i <- 0 until sizeX)
        print(if (grid.contains(Pixel(i, j))) "#" else " ")
      println()
    }
  }

  val emptyGrid: Grid = Set.empty
  val result = input.map(parseInst).foldLeft(emptyGrid)((grid, inst) => inst(grid))

  println(result.size)
  printGrid(result)
}

