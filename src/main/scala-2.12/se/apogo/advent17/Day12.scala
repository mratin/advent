package se.apogo.advent17

import scala.io.Source

object Day12_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input12.txt").getLines.toSeq

  case class Node(n: Int)

  def parseLine(graph: Map[Node, Set[Node]], line: String): Map[Node, Set[Node]] = {
    val split = line.split("<->")
    val node = Node(Integer.parseInt(split(0).trim))
    val neighbours = split(1).split(",").map(_.trim).map(Integer.parseInt).map(Node).toSet
    val updatedGraph = graph.updated(node, graph.getOrElse(node, Set.empty) ++ neighbours)
    neighbours.foldLeft(updatedGraph)((g, n) => g.updated(n, g.getOrElse(n, Set.empty) + node))
  }

  def visit(graph: Map[Node, Set[Node]], toVisit: List[Node], visited: Set[Node]): Set[Node] = {
    toVisit match {
      case Nil => visited
      case node::ns if visited.contains(node) => visit(graph, ns, visited)
      case node::ns =>
        val unvisitedNeighbours = graph(node) -- visited
        visit(graph, ns ++ unvisitedNeighbours.toList, visited + node)
    }
  }

  val graph: Map[Node, Set[Node]] = input.foldLeft(Map[Node, Set[Node]]())(parseLine)

  val result = visit(graph, List(Node(0)), Set.empty)

  println(result.size)
}


object Day12_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input12.txt").getLines.toSeq

  case class Node(n: Int)

  def parseLine(graph: Map[Node, Set[Node]], line: String): Map[Node, Set[Node]] = {
    val split = line.split("<->")
    val node = Node(Integer.parseInt(split(0).trim))
    val neighbours = split(1).split(",").map(_.trim).map(Integer.parseInt).map(Node).toSet
    val updatedGraph = graph.updated(node, graph.getOrElse(node, Set.empty) ++ neighbours)
    neighbours.foldLeft(updatedGraph)((g, n) => g.updated(n, g.getOrElse(n, Set.empty) + node))
  }

  def visit(graph: Map[Node, Set[Node]], toVisit: List[Node], visited: Set[Node]): Set[Node] = {
    toVisit match {
      case Nil => visited
      case node::ns if visited.contains(node) => visit(graph, ns, visited)
      case node::ns =>
        val unvisitedNeighbours = graph(node) -- visited
        visit(graph, ns ++ unvisitedNeighbours.toList, visited + node)
    }
  }

  val graph: Map[Node, Set[Node]] = input.foldLeft(Map[Node, Set[Node]]())(parseLine)

  val groups = graph.keys.groupBy(node => visit(graph, List(node), Set.empty))

  println(groups.size)
}
