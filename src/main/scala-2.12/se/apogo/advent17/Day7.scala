package se.apogo.advent17

import scala.io.Source

object Day7_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input07.txt").getLines.toSeq

  case class Node(name: String, n: Int, children: Seq[String])

  case class Tree(nodes: Set[Node]) {
    def traverse(node: Node): Set[Node] = {
      val children = nodes.filter(n => node.children.contains(n.name))

      children.flatMap(traverse) + node
    }
  }

  def parseNode(line: String): Node = {
    val split = line.split("\\s")
    val name = split.head
    val n = Integer.parseInt(split(1).replace("(", "").replace(")", ""))
    val childSplit = line.split("->")

    val children = if (childSplit.size > 1) {
      childSplit(1).split(",").map(_.trim).toSeq
    } else {
      Nil
    }

    Node(name, n, children)
  }

  val nodes = input.map(parseNode)

  val tree = Tree(nodes.toSet)

  val sizeMap = nodes.map(n => n -> tree.traverse(n).size).toMap
  val root: Node = nodes.maxBy(sizeMap)

  println(root.name)
}



object Day7_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input07.txt").getLines.toSeq

  case class Node(name: String, n: Int, children: Seq[String])

  case class Tree(nodes: Set[Node]) {

    def childNodes(node: Node): Set[Node] = nodes.filter(n => node.children.contains(n.name))

    def traverse(node: Node): Set[Node] = {
      val children = childNodes(node)

      children.flatMap(traverse) + node
    }

    def fullWeight(node: Node): Int = {
      traverse(node).toSeq.map(_.n).sum
    }

    def balance(node: Node): Option[Int] = {
      val childWeights: Map[Node, Int] = childNodes(node).toSeq.map(c => c -> fullWeight(c)).toMap

      if (childWeights.values.toSet.size > 1) {
        val correctWeight: Int = childWeights.values.maxBy(w => childWeights.count(_ == w))
        val (badNode, badNodeFullWeight): (Node, Int) = childWeights.find(_._2 != correctWeight).get
        Some(badNode.n - (badNodeFullWeight - correctWeight))
      } else {
        None
      }
    }
  }

  def parseNode(line: String): Node = {
    val split = line.split("\\s")
    val name = split.head
    val n = Integer.parseInt(split(1).replace("(", "").replace(")", ""))
    val childSplit = line.split("->")

    val children = if (childSplit.size > 1) {
      childSplit(1).split(",").map(_.trim).toSeq
    } else {
      Nil
    }

    Node(name, n, children)
  }

  val nodes = input.map(parseNode)

  val tree = Tree(nodes.toSet)

  val sizeMap = nodes.map(n => n -> tree.traverse(n).size).toMap
  val root: Node = nodes.maxBy(sizeMap)

  val result = nodes.sortBy(sizeMap).flatMap(tree.balance).head

  println(result)
}

