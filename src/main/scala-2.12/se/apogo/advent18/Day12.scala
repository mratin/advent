package se.apogo.advent18

import scala.io.Source

object Day12 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input12.txt").getLines.toSeq

  case class State(plants: Set[Int], minPot: Int, maxPot: Int) {
    override def toString = (minPot to maxPot).map(i => if (plants.contains(i)) '#' else '.').mkString
  }

  val initState = {
    val plants = input.head.split(':').last.trim.zipWithIndex.filter(_._1 == '#').map(_._2).toSet
    State(plants, plants.min-5, plants.max+5)
  }

  val rules: Set[String] = input.drop(2).flatMap(line => {
    val Array(localState, next) = line.split("=>").map(_.trim)
    Some(localState).filter(_ => next == "#")
  }).toSet

  def tick(state: State, ticks: Int): State = {
    if (ticks == 0) state else {
      val nextPlants = {
        for (i <- state.minPot to state.maxPot) yield {
          val is = i-2 to i+2
          val localState = is.map(state.plants.contains).map({case true => '#' case false => '.'}).mkString
          Some(i).filter(_ => rules.contains(localState))
        }}.flatten.toSet

      tick(State(nextPlants, nextPlants.min -5, nextPlants.max +5), ticks-1)
    }
  }

  val result = tick(initState, 20)

  println(result.plants.sum)
}

// Part 2 was solved by interactively running part 1 until finding the pattern
// that lead to the simple expression needed to compute the result
