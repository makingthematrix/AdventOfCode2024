package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines

object DayFour:
  private val XMasArray  = Array('X', 'M', 'A', 'S')
  private val XMasRange  = 0 to 3
  private val Directions = for { x <- -1 to 1; y <- -1 to 1 if (x, y) != (0, 0) } yield (x, y)

  private def getChar(x: Int, y: Int)(using array: Array[Char], len: Int): Option[Char] =
    if x < 0 || y < 0 || x >= len || y >= len then None else Some(array(x * len + y))

  private def checkXMAS(x: Int, y: Int, dir: (Int, Int))(using input: Array[Char], len: Int): Boolean =
    XMasRange.forall(n => getChar(x + (n * dir._1), y + (n * dir._2)).contains(XMasArray(n)))

  private def checkXMAS2(x: Int, y: Int)(using input: Array[Char], len: Int): Boolean = {
    for
      a  <- getChar(x, y)         if a == 'A'
      ul <- getChar(x - 1, y - 1) if ul == 'M' || ul == 'S'
      dr <- getChar(x + 1, y + 1) if (ul == 'S' && dr == 'M') || (ul == 'M' && dr == 'S')
      ur <- getChar(x - 1, y + 1) if ur == 'M' || ur == 'S'
      dl <- getChar(x + 1, y - 1) if (ur == 'S' && dl == 'M') || (ur == 'M' && dl == 'S')
    yield true
  }.getOrElse(false)

  def main(): Unit =
    val lines                = readLines("input4")
    given array: Array[Char] = lines.mkString.toCharArray
    given len: Int           = lines.head.length
    // Part 1
    val range1               = for { x <- 0 until len; y <- 0 until len } yield (x, y)
    val res1                 = range1.map((x, y) => Directions.count(checkXMAS(x, y, _))).sum
    println(s"Part 1: $res1") // 2344
    // Part 2
    val range2               = for { x <- 1 until len - 1; y <- 1 until len - 1 } yield (x, y)
    val res2                 = range2.count(checkXMAS2)
    println(s"Part 2: $res2") // 1815
