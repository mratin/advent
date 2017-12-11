package se.apogo.advent17

import scala.io.Source

object Day11_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input11.txt").getLines.
    toSeq.head.split(",").map(_.trim)

  case class Hex(x: Int, y: Int)

  def go(steps: Seq[String], hex: Hex): Hex = {
    if (steps.isEmpty) hex else {
      val next = steps.head match {
        case "n" => Hex(hex.x,hex.y+1)
        case "ne" => Hex(hex.x+1, hex.y)
        case "se" => Hex(hex.x+1, hex.y-1)
        case "s" => Hex(hex.x, hex.y-1)
        case "sw" => Hex(hex.x-1, hex.y)
        case "nw" => Hex(hex.x-1, hex.y+1)
      }
      go(steps.tail, next)
    }
  }

  def distance(hex: Hex): Int = {
    math.min(math.abs(hex.x) + math.abs(hex.y),
      math.min(hex.x, hex.y) + math.abs(hex.x - hex.y))
  }

  val result = distance(go(input, Hex(0,0)))

  println(result)
}


object Day11_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input11.txt").getLines.
    toSeq.head.split(",").map(_.trim)

  case class Hex(x: Int, y: Int)

  def go(steps: Seq[String], hex: Hex, furthest: Hex): Hex = {
    if (steps.isEmpty) furthest else {
      val next = steps.head match {
        case "n" => Hex(hex.x,hex.y+1)
        case "ne" => Hex(hex.x+1, hex.y)
        case "se" => Hex(hex.x+1, hex.y-1)
        case "s" => Hex(hex.x, hex.y-1)
        case "sw" => Hex(hex.x-1, hex.y)
        case "nw" => Hex(hex.x-1, hex.y+1)
      }
      val newFurthest = if (distance(furthest) > distance(next)) furthest else next
      go(steps.tail, next, newFurthest)
    }
  }

  def distance(hex: Hex): Int = {
    math.min(math.abs(hex.x) + math.abs(hex.y),
      math.min(hex.x, hex.y) + math.abs(hex.x - hex.y))
  }

  val result = distance(go(input, Hex(0,0), Hex(0,0)))

  println(result)
}
