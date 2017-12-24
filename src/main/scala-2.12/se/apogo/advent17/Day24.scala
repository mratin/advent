package se.apogo.advent17

import scala.io.Source

object Day24_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input24.txt").getLines.toSeq

  case class Node(id: Int, from: Int, to: Int) {
    def strength = from + to
    def flip = copy(from = to, to = from)
  }

  case class Path(nodes: Seq[Node]) {
    def strength = nodes.map(_.strength).sum
  }

  def search(nodeMap: Map[Int, Set[Node]], path: List[Node], usedIds: Set[Int]): Set[Path] = {
    val nexts: Set[Node] = nodeMap.getOrElse(path.head.to, Set.empty).filter(n => !usedIds.contains(n.id))
    val nextPaths: Set[Path] = nexts.flatMap(next => search(nodeMap, next::path, usedIds + next.id))

    if (nextPaths.isEmpty) {
      Set(Path(path))
    } else {
      nextPaths
    }
  }

  val nodes = input.zipWithIndex.map({ case (s,i) => {
    val Array(from, to) = s.split("/").map(_.trim.toInt)
    Node(i, from, to)
  }})

  val nodesWithFlips = nodes ++ nodes.map(_.flip)
  val nodeMap: Map[Int, Set[Node]] = nodesWithFlips.groupBy(_.from).mapValues(_.toSet)

  val paths = search(nodeMap, List(Node(-1,0,0)), Set.empty)

  println(paths.map(_.strength).max)
}

object Day24_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input24.txt").getLines.toSeq

  case class Node(id: Int, from: Int, to: Int) {
    def strength = from + to
    def flip = copy(from = to, to = from)
  }

  case class Path(nodes: Seq[Node]) {
    def strength = nodes.map(_.strength).sum
    def length = nodes.size
  }

  def search(nodeMap: Map[Int, Set[Node]], path: List[Node], usedIds: Set[Int]): Set[Path] = {
    val nexts: Set[Node] = nodeMap.getOrElse(path.head.to, Set.empty).filter(n => !usedIds.contains(n.id))
    val nextPaths: Set[Path] = nexts.flatMap(next => search(nodeMap, next::path, usedIds + next.id))

    if (nextPaths.isEmpty) {
      Set(Path(path))
    } else {
      nextPaths
    }
  }

  val nodes = input.zipWithIndex.map({ case (s,i) => {
    val Array(from, to) = s.split("/").map(_.trim.toInt)
    Node(i, from, to)
  }})

  val nodesWithFlips = nodes ++ nodes.map(_.flip)
  val nodeMap: Map[Int, Set[Node]] = nodesWithFlips.groupBy(_.from).mapValues(_.toSet)

  val paths: Set[Path] = search(nodeMap, List(Node(-1,0,0)), Set.empty)

  val longest = paths.toSeq.sortBy(-_.strength).sortBy(-_.length)

  println(longest.head.strength)
}
