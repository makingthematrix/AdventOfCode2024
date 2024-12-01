package io.github.makingthematrix.AdventofCode2024

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

object DayOne {
  private val path = Path.of("resources/input1")
  private val linePattern: Regex = """^(\d+)\s+(\d+)$""".r

  private def readInput: (Seq[Int], Seq[Int]) = {
    val pairs = Files.readAllLines(path).asScala.toSeq.collect { case linePattern(a, b) => (a.toInt, b.toInt) }
    (pairs.map(_._1), pairs.map(_._2))
  }

  @main def main(): Unit = {
    val (left, right) = readInput
    // Phase 1
    val phase1 = left.sorted.zip(right.sorted).map((a, b) => math.abs(a - b)).sum
    println(s"Phase 1: $phase1") // 3569916
    // Phase 2
    val phase2 = left.map { a => a * right.count(_ == a) }.sum
    println(s"Phase 2: $phase2") // 26407426
  }
}
