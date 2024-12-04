package io.github.makingthematrix.AdventofCode2024

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

object DayOne:
  private val abRegex: Regex = """^(\d+)\s+(\d+)$""".r

  def main(): Unit =
    val (left, right) = {
      val pairs = Files.readAllLines(Path.of("resources/input1")).asScala.toSeq.collect { case abRegex(a, b) => (a.toInt, b.toInt) }
      (pairs.map(_._1), pairs.map(_._2))
    }
    println(s"Phase 1: ${left.sorted.zip(right.sorted).map((a, b) => math.abs(a - b)).sum}")
    println(s"Phase 2: ${left.map { a => a * right.count(_ == a) }.sum}")
