package se.apogo.advent18

import scala.io.Source

object Day10 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input10.txt").getLines.toSeq

  case class Vec(x: Int, y: Int) {
    def add(v: Vec) = Vec(x+v.x, y+v.y)
  }

  def parse(line: String): (Vec, Vec) = {
    val Array(_, p, v) = line.split('<')
    val Array(px,py) = p.split(',')
    val Array(vx,vy) = v.split(',')
    val pos = Vec(Integer.parseInt(px.trim), Integer.parseInt(py.trim.takeWhile(_ != '>')))
    val vel = Vec(Integer.parseInt(vx.trim), Integer.parseInt(vy.trim.takeWhile(_ != '>')))
    pos -> vel
  }

  def tick(ps: Seq[(Vec, Vec)], s: Int): Int = {
    val minX = ps.map(_._1.x).min
    val minY = ps.map(_._1.y).min
    val maxX = ps.map(_._1.x).max
    val maxY = ps.map(_._1.y).max

    if (maxY - minY < 10) {
      val positions = ps.map(_._1).toSet
      for (y <- minY to maxY) {
        for (x <- minX to maxX)
          print(if (positions.contains(Vec(x,y))) "#" else " ")
        println
      }
      s
    } else
      tick(ps.map(p => (p._1.add(p._2), p._2)), s + 1)
  }

  println(tick(input.map(parse), 0))
}

