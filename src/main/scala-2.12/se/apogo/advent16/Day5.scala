package se.apogo.advent16

import java.security.MessageDigest

import scala.util.Try

object Day5_1 extends App {
  val input: String = "cxdnnyjw"
  val messageDigest = MessageDigest.getInstance("MD5")

  def getDigit(i: Int): Option[Char] = {
    val hash = messageDigest.digest((input + i).getBytes).
      map(byte => "%02x".format(byte & 0xff)).mkString
    if (hash.startsWith("00000")) {
      Some(hash(5))
    } else {
      None
    }
  }

  val result = Stream.from(0).flatMap(getDigit).take(8).mkString
  println(result)
}

object Day5_2 extends App {
  val input: String = "cxdnnyjw"
  val messageDigest = MessageDigest.getInstance("MD5")

  def decrypt(i: Int, password: Seq[Option[Char]]): Seq[Char] = {
    if (password.forall(_.isDefined)) {
      password.flatten.mkString
    } else {
      val hash = messageDigest.digest((input + i).getBytes).
        map(byte => "%02x".format(byte & 0xff)).mkString

      if (hash.startsWith("00000")) {
        def isValid(pos: Int) = password.isDefinedAt(pos) && password(pos).isEmpty

        Try(hash(5).toString.toInt).toOption match {
          case Some(position) if isValid(position) =>
            val newPassword = password.updated(position, Some(hash(6)))

            println(newPassword.map(_.getOrElse("_")).mkString)
            decrypt(i+1, newPassword)

          case None => {
            println("Ignored position: " + hash(5))
            decrypt(i+1, password)
          }
        }
      } else {
        decrypt(i+1, password)
      }
    }
  }

  val result = decrypt(0, Seq.fill(8)(None))
  println(result)
}


