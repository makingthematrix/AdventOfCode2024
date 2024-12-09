package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.{Dir, Pos, getChar, readLines}

object DayFour:
  private val XMasArray            = Array('X', 'M', 'A', 'S')
  private val XMasRange            = 0 to 3
  private val Directions: Seq[Dir] = for { x <- -1 to 1; y <- -1 to 1 if (x, y) != (0, 0) } yield Dir(x, y)

  private def checkXMAS(pos: Pos, dir: Dir)(using input: Array[Char], len: Int): Boolean =
    XMasRange.forall(n => (pos + (dir * n)).toChar.contains(XMasArray(n)))

  private def checkXMAS2(pos: Pos)(using input: Array[Char], len: Int): Boolean = {
    for
      a  <- getChar(pos.x    , pos.y    ) if a == 'A'
      ul <- getChar(pos.x - 1, pos.y - 1) if ul == 'M' || ul == 'S'
      dr <- getChar(pos.x + 1, pos.y + 1) if (ul == 'S' && dr == 'M') || (ul == 'M' && dr == 'S')
      ur <- getChar(pos.x - 1, pos.y + 1) if ur == 'M' || ur == 'S'
      dl <- getChar(pos.x + 1, pos.y - 1) if (ur == 'S' && dl == 'M') || (ur == 'M' && dl == 'S')
    yield true
  }.getOrElse(false)

  def main(): Unit =
    val lines                = readLines("input4")
    given array: Array[Char] = lines.mkString.toCharArray
    given len: Int           = lines.head.length
    // Part 1
    val range1: Seq[Pos]     = for { x <- 0 until len; y <- 0 until len } yield Pos(x, y)
    val res1                 = range1.map(pos => Directions.count(checkXMAS(pos, _))).sum
    println(s"Part 1: $res1") // 2344
    // Part 2
    val range2: Seq[Pos]     = for { x <- 1 until len - 1; y <- 1 until len - 1 } yield Pos(x, y)
    val res2                 = range2.count(checkXMAS2)
    println(s"Part 2: $res2") // 1815
