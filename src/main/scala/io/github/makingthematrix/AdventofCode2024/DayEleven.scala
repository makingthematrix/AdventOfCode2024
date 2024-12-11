package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readString
import scala.collection.mutable.HashMap

object DayEleven:
  @main def main(): Unit =
    val numbers = readString("input11").split(" ").map(_.toLong)
    // Part 1
    val res1 = numbers.map(blink(_, 25)).sum
    println(s"Part 1: $res1") // 183484
    // Part 2
    val res2 = numbers.map(blink(_, 75)).sum
    println(s"Part 2: $res2") // 218817038947400

  object EvenCiphers:
    private def countCiphers(n: Long): Int =
      var i = n
      var count = 0
      while i > 0 do
        count += 1
        i /= 10
      count

    def unapply(n: Long): Option[(Long, Long)] =
      val ciphersNumber = countCiphers(n)
      if ciphersNumber % 2 == 1 then None else
        val half = (0 until ciphersNumber / 2).foldLeft(1L) { (n, _) => n * 10L }
        Some((n / half, n % half))

  private def blink(number: Long, n: Int)(using map: HashMap[(Long, Int), Long] = HashMap[(Long, Int), Long]()) : Long =
    if n == 0 then 1L else map.getOrElseUpdate((number, n), number match {
      case 0L                => blink(1L, n - 1)
      case EvenCiphers(a, b) => blink(a, n - 1) + blink(b, n - 1)
      case _                 => blink(number * 2024L, n - 1)
    })
