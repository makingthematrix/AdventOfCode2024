package io.github.makingthematrix.AdventofCode2024

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object DayFour:
  private val XmasArray = Array('X', 'M', 'A', 'S')
  private val Directions = for { x <- -1 to 1; y <- -1 to 1 if (x, y) != (0, 0) } yield (x, y)

  private def getChar(x: Int, y: Int)(using array: Array[Char], len: Int): Option[Char] =
    if x < 0 || y < 0 || x >= len || y >= len then None else Some(array(x * len + y))

  private def checkXMAS(x: Int, y: Int, dir: (Int, Int))(using input: Array[Char], len: Int): Boolean =
    (0 to 3).forall { n => getChar(x + (n * dir._1), y + (n * dir._2)).contains(XmasArray(n)) }

  private def checkXMAS2(x: Int, y: Int)(using input: Array[Char], len: Int): Boolean = {
    for
      a  <- getChar(x, y)         if a == 'A'
      ul <- getChar(x - 1, y - 1) if ul == 'M' || ul == 'S'
      ur <- getChar(x - 1, y + 1) if ur == 'M' || ur == 'S'
      dl <- getChar(x + 1, y - 1) if (ur == 'S' && dl == 'M') || (ur == 'M' && dl == 'S')
      dr <- getChar(x + 1, y + 1) if (ul == 'S' && dr == 'M') || (ul == 'M' && dr == 'S')
    yield true
  }.getOrElse(false)

  @main def main(): Unit =
    val lines = Files.readAllLines(Path.of("resources/input4")).asScala.toSeq
    given array: Array[Char] = lines.mkString.toCharArray
    given len: Int = lines.head.length
    val range = for { x <- 0 until len; y <- 0 until len } yield (x, y)
    // Part 1
    val res1 = range.map { (x, y) => Directions.count(checkXMAS(x, y, _)) }.sum
    println(res1) // 2344
    // Part 2
    val res2 = range.count(checkXMAS2)
    println(res2) // 1815
