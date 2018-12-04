package se.apogo.advent18

import scala.io.Source

object Day04 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input04.txt").getLines.map(_.trim).toSeq

  case class Event(min: Int, guard: Int)

  def parse(lines: List[String], currentGuard: Option[Int]): List[Event] = lines match {
    case Nil => Nil
    case line::tail =>
      val min = Integer.parseInt(line.substring(15,17))
      line.drop(19).split(" ") match {
        case Array("Guard", id, "begins", "shift") => parse(tail, Some(Integer.parseInt(id.tail)))
        case _ => Event(min, currentGuard.get) :: parse(tail, currentGuard)
      }}

  def asleep(shift: List[Event]): Int = shift.sliding(2,2)
    .map({case List(asleep, wake) => wake.min - asleep.min}).sum

  val guardShifts: Map[Int, List[Event]] = parse(input.sorted.toList, None).groupBy(_.guard)
  val guard: Int = guardShifts.keys.maxBy(guardShifts.mapValues(asleep))
  val minutesSleep: Int = (0 to 59).maxBy(m => guardShifts(guard).sliding(2,2)
    .count({case List(asleep, awake) => asleep.min <= m && m < awake.min}))

  println(guard * minutesSleep)
}

object Day04_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input04.txt").getLines.map(_.trim).toSeq

  case class Event(min: Int, guard: Int)

  def parse(lines: List[String], currentGuard: Option[Int]): List[Event] = lines match {
    case Nil => Nil
    case line :: tail =>
      val min = Integer.parseInt(line.substring(15, 17))
      line.drop(19).split(" ") match {
        case Array("Guard", id, "begins", "shift") => parse(tail, Some(Integer.parseInt(id.tail)))
        case _ => Event(min, currentGuard.get) :: parse(tail, currentGuard)
      }
  }

  def asleep(shift: List[Event]): Int = shift.sliding(2, 2)
    .map({ case List(asleep, wake) => wake.min - asleep.min }).sum

  val guardShifts: Map[Int, List[Event]] = parse(input.sorted.toList, None).groupBy(_.guard)
  val guards = guardShifts.keys

  def minutesSleep(guard: Int): Map[Int,Int] = (0 to 59).map(m => m -> guardShifts(guard).sliding(2, 2)
    .count({ case List(asleep, awake) => asleep.min <= m && m < awake.min })).toMap

  val guardMostFreqMap: Map[Int, (Int, Int)] = guards.map(g => g -> minutesSleep(g).maxBy(_._2)).toMap
  val guardAndMostFreqMin: (Int, (Int, Int)) = guardMostFreqMap.maxBy(_._2._2)

  println(guardAndMostFreqMin._1 * guardAndMostFreqMin._2._1)
}
