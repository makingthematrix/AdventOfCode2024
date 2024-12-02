package io.github.makingthematrix.AdventofCode2024

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

object DayOne:
  private val linePattern: Regex = """^(\d+)\s+(\d+)$""".r

  private def readInput: (Seq[Int], Seq[Int]) =
    val pairs =
      Files.readAllLines(Path.of("resources/input1")).asScala.toSeq.collect { case linePattern(a, b) => (a.toInt, b.toInt) }
    (pairs.map(_._1), pairs.map(_._2))

  def main(): Unit =
    val (left, right) = readInput
    println(s"Phase 1: ${left.sorted.zip(right.sorted).map((a, b) => math.abs(a - b)).sum}") // 3569916
    println(s"Phase 2: ${left.map { a => a * right.count(_ == a) }.sum}") // 26407426
