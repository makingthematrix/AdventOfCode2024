package io.github.makingthematrix.AdventofCode2024

import java.nio.file.{Files, Path}
import scala.util.matching.Regex

object DayThree:
  private def readInput: String = Files.readString(Path.of("resources/input3"))
  private val mulPattern: Regex = """mul\((\d+)\,(\d+)\)""".r
  private val allPattern: Regex = """(mul\((\d+)\,(\d+)\)|do\(\)|don\'t\(\))""".r

  @main def main(): Unit =
    val str = readInput
    // Part 1
    val res1 = mulPattern.findAllIn(str).collect { case mulPattern(a, b) => a.toInt * b.toInt }.sum
    println(res1)
    // Part 2
    val (_, res2) = allPattern.findAllIn(str).foldLeft((true, 0)) {
      case ((flag, sum), "don't()")        => (false, sum)
      case ((flag, sum), "do()")           => (true, sum)
      case ((true, sum), mulPattern(a, b)) => (true, sum + (a.toInt * b.toInt))
      case ((flag, sum), _)                => (flag, sum)
    }
    println(res2)
