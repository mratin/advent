package se.apogo.advent18

import scala.io.Source

object Day08 extends App {
  val input: Seq[Int] = Source.fromResource("se/apogo/advent18/input08.txt").getLines
    .toSeq.head.split("\\s").map(Integer.parseInt)

  case class Node(children: Seq[Node], meta: Seq[Int])

  def parse(numbers: Seq[Int], toParse: Int, nodes: Seq[Node]): (Seq[Node], Seq[Int]) = {
    if (toParse == 0) (nodes, numbers) else {
      val nrChildren = numbers.head
      val nrMetas = numbers(1)

      val (children, rest) = parse(numbers.drop(2), nrChildren, Nil)
      val parsedNode = Node(children, rest.take(nrMetas))

      parse(rest.drop(nrMetas), toParse-1, nodes :+ parsedNode)
    }
  }

  val tree = parse(input, 1, Nil)._1.head
  def sumOfMetas(node: Node): Int = node.meta.sum + node.children.map(sumOfMetas).sum

  println(sumOfMetas(tree))
}

object Day08_2 extends App {
  val input: Seq[Int] = Source.fromResource("se/apogo/advent18/input08.txt").getLines
    .toSeq.head.split("\\s").map(Integer.parseInt)

  case class Node(children: Seq[Node], meta: Seq[Int])

  def parse(numbers: Seq[Int], toParse: Int, nodes: Seq[Node]): (Seq[Node], Seq[Int]) = {
    if (toParse == 0) (nodes, numbers) else {
      val nrChildren = numbers.head
      val nrMetas = numbers(1)

      val (children, rest) = parse(numbers.drop(2), nrChildren, Nil)
      val parsedNode = Node(children, rest.take(nrMetas))

      parse(rest.drop(nrMetas), toParse-1, nodes :+ parsedNode)
    }
  }

  val tree = parse(input, 1, Nil)._1.head

  def value(node: Node): Int =
    if (node.children.isEmpty) node.meta.sum
    else node.meta.flatMap(i => node.children.lift(i-1)).map(value).sum

  println(value(tree))
}
