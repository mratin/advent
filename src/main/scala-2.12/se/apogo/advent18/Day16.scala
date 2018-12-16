package se.apogo.advent18

import scala.io.Source

object Day16 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input16.txt").getLines.toSeq

  type State = Map[Int, Int]
  type Ins = State => Opcode => State

  case class Opcode(n: Int, a: Int, b: Int, c: Int)

  def op(f: (State,Int,Int) => Int) = (r: State) => (o: Opcode) => r.updated(o.c, f(r, o.a, o.b))

  val addr = op((r,a,b) => r(a) + r(b))
  val addi = op((r,a,b) => r(a) + b)
  val mulr = op((r,a,b) => r(a) * r(b))
  val muli = op((r,a,b) => r(a) * b)
  val banr = op((r,a,b) => r(a) & r(b))
  val bani = op((r,a,b) => r(a) & b)
  val borr = op((r,a,b) => r(a) | r(b))
  val bori = op((r,a,b) => r(a) | b)
  val setr = op((r,a,b) => r(a))
  val seti = op((r,a,b) => a)
  val gtir = op((r,a,b) => if (a > r(b)) 1 else 0)
  val gtri = op((r,a,b) => if (r(a) > b) 1 else 0)
  val gtrr = op((r,a,b) => if (r(a) > r(b)) 1 else 0)
  val eqir = op((r,a,b) => if (a == r(b)) 1 else 0)
  val eqri = op((r,a,b) => if (r(a) == b) 1 else 0)
  val eqrr = op((r,a,b) => if (r(a) == r(b)) 1 else 0)

  val is = Seq(addr,addi,mulr,muli,banr,bani,borr,bori,setr,seti,gtir,gtri,gtrr,eqir,eqri,eqrr)

  def count(lines: Seq[String]): Int = {
    def parseState(line: String) = line.drop(9).dropRight(1).split(",").map(_.trim.toInt).zipWithIndex.map(_.swap).toMap
    val in = parseState(lines.head)
    val out = parseState(lines(2))
    val Array(o,a,b,c) = lines(1).split("\\s").map(_.trim.toInt)
    is.map(_(in)).count(_(Opcode(o,a,b,c)) == out)
  }

  val part1 = input.filterNot(_.trim.isEmpty).sliding(3,3).takeWhile(_.head.startsWith("Before"))
  println(part1.map(count).count(_ >= 3))
}

object Day16_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input16.txt").getLines.toSeq

  type State = Map[Int, Int]
  type Ins = State => Opcode => State

  case class Opcode(n: Int, a: Int, b: Int, c: Int)

  def op(f: (State,Int,Int) => Int) = (r: State) => (o: Opcode) => r.updated(o.c, f(r, o.a, o.b))

  val addr = op((r,a,b) => r(a) + r(b))
  val addi = op((r,a,b) => r(a) + b)
  val mulr = op((r,a,b) => r(a) * r(b))
  val muli = op((r,a,b) => r(a) * b)
  val banr = op((r,a,b) => r(a) & r(b))
  val bani = op((r,a,b) => r(a) & b)
  val borr = op((r,a,b) => r(a) | r(b))
  val bori = op((r,a,b) => r(a) | b)
  val setr = op((r,a,b) => r(a))
  val seti = op((r,a,b) => a)
  val gtir = op((r,a,b) => if (a > r(b)) 1 else 0)
  val gtri = op((r,a,b) => if (r(a) > b) 1 else 0)
  val gtrr = op((r,a,b) => if (r(a) > r(b)) 1 else 0)
  val eqir = op((r,a,b) => if (a == r(b)) 1 else 0)
  val eqri = op((r,a,b) => if (r(a) == b) 1 else 0)
  val eqrr = op((r,a,b) => if (r(a) == r(b)) 1 else 0)

  val is = Set(addr,addi,mulr,muli,banr,bani,borr,bori,setr,seti,gtir,gtri,gtrr,eqir,eqri,eqrr)

  case class Sample(before: State, opcode: Opcode, after: State)

  def parseOpcode(line: String): Opcode = {
    val Array(o,a,b,c) = line.split("\\s").map(_.trim.toInt)
    Opcode(o,a,b,c)
  }

  def parse(lines: Seq[String]): Sample = {
    def parseState(line: String) = line.drop(9).dropRight(1).split(",").map(_.trim.toInt).zipWithIndex.map(_.swap).toMap
    Sample(parseState(lines.head), parseOpcode(lines(1)), parseState(lines(2)))
  }

  val samples: Seq[Sample] = input.filterNot(_.trim.isEmpty).sliding(3,3).takeWhile(_.head.startsWith("Before")).toSeq.map(parse)

  val opcodeMap: Map[Int, Ins] = {
    def matches(sample: Sample)(ins: Ins): Boolean = ins(sample.before)(sample.opcode) == sample.after

    def solve(acc: Map[Int, Set[Ins]], sample: Sample): Map[Int, Set[Ins]] = {
      val newMap = acc.updated(sample.opcode.n, acc(sample.opcode.n).filter(matches(sample)))
      val unique: Seq[Int] = newMap.filter(_._2.size == 1).keys.toSeq
      unique.foldLeft(newMap)((a, u) => a.map({case (k,ins) => k -> (if (u != k) ins -- a(u) else ins)}))
    }

    val init: Map[Int, Set[Ins]] = (0 to 15).map(i => i -> is).toMap
    samples.foldLeft(init)(solve).mapValues(_.head)
  }

  val program: Seq[Opcode] = input.drop(input.lastIndexWhere(_.startsWith("After"))+1).dropWhile(_.trim.isEmpty).map(parseOpcode)

  def exec(r: State, opcode: Opcode): State = opcodeMap(opcode.n)(r)(opcode)

  val result = program.foldLeft((0 to 3).map(i => i -> 0).toMap)(exec)

  println(result(0))
}
