package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.{Dir, Pos, readLines, getChar}

object DayTen:
  private val Directions: Array[Dir] = Array(Dir(-1, 0), Dir(1, 0), Dir(0, -1), Dir(0, 1))

  @main def main(): Unit =
    val lines                = readLines("input10")
    given array: Array[Char] = lines.mkString.toCharArray
    given len: Int           = lines.head.length
    val trailheads           = array.zipWithIndex.collect { case (c, index) if c == '0' => Pos.fromIndex(index) }
    // Part 1
    val res1 = trailheads.map(trail1(_).size).sum
    println(s"Part 1: $res1") // 776
    // Part 2
    val res2 = trailheads.map(trail2(_)).sum
    println(s"Part 2: $res2") // 1657

  private def trail1(pos: Pos, lastChar: Char = ('0' - 1).toChar)(using array: Array[Char], len: Int): Set[Pos] = getChar(pos) match
    case None                         => Set.empty
    case Some(c) if c - lastChar != 1 => Set.empty
    case Some('9')                    => Set(pos)
    case Some(c)                      => Directions.map(dir => trail1(pos + dir, c)).reduce(_ ++ _)

  private def trail2(pos: Pos, lastChar: Char = ('0' - 1).toChar)(using array: Array[Char], len: Int): Int = getChar(pos) match
    case None                         => 0
    case Some(c) if c - lastChar != 1 => 0
    case Some('9')                    => 1
    case Some(c)                      => Directions.map(dir => trail2(pos + dir, c)).sum
