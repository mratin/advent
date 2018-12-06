package se.apogo.advent18

import scala.io.Source

object Day06 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input06.txt").getLines.toSeq

  case class Pos(x:Int,y:Int) {
    def distanceTo(pos: Pos) = math.abs(pos.x - x) + math.abs(pos.y - y)
  }

  def parse(line: String) = {
    val Array(x,y) = line.split(",").map(_.trim).map(Integer.parseInt)
    Pos(x,y)
  }

  val coords: Seq[Pos] = input.map(parse)
  val minX = coords.map(_.x).min
  val minY = coords.map(_.y).min
  val maxX = coords.map(_.x).max
  val maxY = coords.map(_.y).max

  val allPositions: Seq[Pos] = for (i <- minX to maxX; j <- minY to maxY) yield Pos(i, j)

  val positionNearestCoord: Map[Pos, Option[Pos]] = allPositions.map(p => p -> {
    val distances: Map[Pos, Int] = coords.map(g => g -> g.distanceTo(p)).toMap
    val minDist = distances.values.min
    val poss = distances.filter(_._2 == minDist).keys
    if (poss.size == 1) Some(poss.head) else None
  }).toMap

  def area(coord: Pos): Set[Pos] = allPositions.filter(p => positionNearestCoord(p).contains(coord)).toSet
  def isFinite(area: Set[Pos]) = area.forall(p => minX < p.x && p.x < maxX && minY < p.y && p.y < maxY)

  val largestFinite = coords.map(area).filter(isFinite).map(_.size).max

  println(largestFinite)
}

object Day06_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input06.txt").getLines.toSeq

  case class Pos(x:Int,y:Int) {
    def distanceTo(pos: Pos) = math.abs(pos.x - x) + math.abs(pos.y - y)
  }

  def parse(line: String) = {
    val Array(x,y) = line.split(",").map(_.trim).map(Integer.parseInt)
    Pos(x,y)
  }

  val coords: Seq[Pos] = input.map(parse)
  val minX = coords.map(_.x).min
  val minY = coords.map(_.y).min
  val maxX = coords.map(_.x).max
  val maxY = coords.map(_.y).max

  val allPositions: Seq[Pos] = for (i <- minX to maxX; j <- minY to maxY) yield Pos(i, j)
  val posCordDistSum: Map[Pos, Int] = allPositions.map(p => p -> coords.map(_.distanceTo(p)).sum).toMap
  val result = allPositions.filter(p => posCordDistSum(p) < 10000)

  println(result.size)
}
