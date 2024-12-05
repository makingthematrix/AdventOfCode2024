package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines
import scala.util.matching.Regex

object DayOne:
  private val abRegex: Regex = """^(\d+)\s+(\d+)$""".r

  def main(): Unit =
    val (left, right) =
      val pairs = readLines("input1").collect { case abRegex(a, b) => (a.toInt, b.toInt) }
      (pairs.map(_._1), pairs.map(_._2))
    println(s"Part 1: ${left.sorted.zip(right.sorted).map((a, b) => math.abs(a - b)).sum}")
    println(s"Part 2: ${left.map(a => a * right.count(_ == a)).sum}")
