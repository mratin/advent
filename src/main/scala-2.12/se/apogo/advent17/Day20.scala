package se.apogo.advent17

import scala.io.Source

object Day20_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input20.txt").getLines.toSeq

  case class Vec(x: Int, y: Int, z: Int) {
    def dist = math.abs(x) + math.abs(y) + math.abs(z)
  }

  case class Particle(position: Vec, velocity: Vec, acceleration: Vec)

  def parseVec(string: String): Vec = {
    val Array(x,y,z) = string.dropWhile(_ != '<').tail.takeWhile(_ != '>').split(',').map(_.trim.toInt)
    Vec(x,y,z)
  }

  def parseParticle(line: String): Particle = {
    val Array(p,v,a) = line.split(", ").map(parseVec)
    Particle(p, v, a)
  }

  val particles = input.map(parseParticle)

  val result = particles.sortWith((p1,p2) => {
    if (p1.acceleration.dist == p2.acceleration.dist) {
      if (p1.velocity.dist == p1.velocity.dist) {
        p1.position.dist < p2.position.dist
      } else {
        p1.velocity.dist < p2.velocity.dist
      }
    } else {
      p1.acceleration.dist < p2.acceleration.dist
    }
  })

  println(particles.indexOf(result.head))
}

object Day20_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input20.txt").getLines.toIndexedSeq

  case class Vec(x: Int, y: Int, z: Int) {
    def +(v: Vec) = Vec(x + v.x, y + v.y, z + v.z)
  }

  case class Particle(position: Vec, velocity: Vec, acceleration: Vec) {
    def next = {
      val v = velocity + acceleration
      copy(position = position + v, velocity = v)
    }
  }

  def parseVec(string: String): Vec = {
    val Array(x,y,z) = string.dropWhile(_ != '<').tail.takeWhile(_ != '>').split(',').map(_.trim.toInt)
    Vec(x,y,z)
  }

  def parseParticle(line: String): Particle = {
    val Array(p,v,a) = line.split(", ").map(parseVec)
    Particle(p,v,a)
  }

  def tick(particles: Seq[Particle]): Seq[Particle] = {
    val pMap = particles.groupBy(_.position)
    particles.filterNot(p => pMap(p.position).size > 1).map(_.next)
  }

  val particles: Seq[Particle] = input.map(parseParticle)
  val result = (0 to 10000).foldLeft(particles)((p,_) => tick(p))

  println(result.size)
}

