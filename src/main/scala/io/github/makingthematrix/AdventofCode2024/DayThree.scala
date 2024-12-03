package io.github.makingthematrix.AdventofCode2024

import java.nio.file.{Files, Path}
import scala.util.matching.Regex

object DayThree:
  private val mulPattern: Regex = """mul\((\d+),(\d+)\)""".r
  private val allPattern: Regex = """(mul\((\d+),(\d+)\)|do\(\)|don\'t\(\))""".r

  @main def main(): Unit =
    val input = Files.readString(Path.of("resources/input3"))
    // Part 1
    val res1 = mulPattern.findAllIn(input).collect { case mulPattern(a, b) => a.toInt * b.toInt }.sum
    println(res1)
    // Part 2
    val (_, res2) = allPattern.findAllIn(input).foldLeft((true, 0)) {
      case ((true, sum), mulPattern(a, b)) => (true, sum + (a.toInt * b.toInt))
      case ((_, sum), "don't()")           => (false, sum)
      case ((_, sum), "do()")              => (true, sum)
      case ((flag, sum), _)                => (flag, sum)
    }
    println(res2)
