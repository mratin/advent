package se.apogo.advent17

import scala.io.Source

object Day19_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input19.txt").getLines.toSeq

  case class Vec(x:Int, y:Int) {
    def +(v: Vec) = Vec(x+v.x, y+v.y)
  }

  case class Node(loc: Vec, tag: Option[Char]) {
    def neighbours(world: World): Set[(Vec, Node)] = {
      Set(Vec(1,0), Vec(0,1), Vec(-1,0), Vec(0,-1)).flatMap(dir => {
        world.get(loc + dir).map(node => dir -> node)
      })
    }
  }

  type World = Map[Vec, Node]

  def parseWorldLine(world: World, lineIndex: (String,Int)): World = {
    val (line, y) = lineIndex
    line.zipWithIndex.foldLeft(world)((world: World, ci) => {
      val (c, x) = ci
      val loc = Vec(x,y)
      if ("|+-".contains(c)) {
        world.updated(loc, Node(loc, None))
      } else if (c.isLetter) {
        world.updated(loc, Node(loc, Some(c)))
      } else {
        world
      }
    })
  }

  def go(world: World, from: Node, direction: Vec, visited: Seq[Char]): Seq[Char] = {
    val neighbours: Set[(Vec, Node)] = from.neighbours(world)

    val newVisited = from.tag.map(tag => visited :+ tag).getOrElse(visited)

    neighbours.find(_._1 == direction) match {
      case Some((_, next)) => go(world, next, direction, newVisited)
      case None =>
        neighbours.find(n => n._1 + direction != Vec(0,0)) match {
          case Some((newDirection, next)) => go(world, next, newDirection, newVisited)
          case None => newVisited
        }
    }
  }

  val world = input.zipWithIndex.foldLeft(Map[Vec, Node]())(parseWorldLine)

  val start = world.values.find(node => node.loc.y == 0).get
  val result = go(world, start, Vec(0,1), Nil)

  println(result.mkString)
}


object Day19_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input19.txt").getLines.toSeq

  case class Vec(x:Int, y:Int) {
    def +(v: Vec) = Vec(x+v.x, y+v.y)
  }

  case class Node(loc: Vec, tag: Option[Char]) {
    def neighbours(world: World): Set[(Vec, Node)] = {
      Set(Vec(1,0), Vec(0,1), Vec(-1,0), Vec(0,-1)).flatMap(dir => {
        world.get(loc + dir).map(node => dir -> node)
      })
    }
  }

  type World = Map[Vec, Node]

  def parseWorldLine(world: World, lineIndex: (String,Int)): World = {
    val (line, y) = lineIndex
    line.zipWithIndex.foldLeft(world)((world: World, ci) => {
      val (c, x) = ci
      val loc = Vec(x,y)
      if ("|+-".contains(c)) {
        world.updated(loc, Node(loc, None))
      } else if (c.isLetter) {
        world.updated(loc, Node(loc, Some(c)))
      } else {
        world
      }
    })
  }

  def go(world: World, from: Node, direction: Vec, visited: Seq[Char], steps: Int): Int = {
    val neighbours: Set[(Vec, Node)] = from.neighbours(world)

    val newVisited = from.tag.map(tag => visited :+ tag).getOrElse(visited)

    neighbours.find(_._1 == direction) match {
      case Some((_, next)) => go(world, next, direction, newVisited, steps+1)
      case None =>
        neighbours.find(n => n._1 + direction != Vec(0,0)) match {
          case Some((newDirection, next)) => go(world, next, newDirection, newVisited, steps+1)
          case None => steps
        }
    }
  }

  val world = input.zipWithIndex.foldLeft(Map[Vec, Node]())(parseWorldLine)

  val start = world.values.find(node => node.loc.y == 0).get
  val result = go(world, start, Vec(0,1), Nil, 1)

  println(result)
}
