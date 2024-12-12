package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.{Dir, Pos, readLines, getChar, Directions}

object DayTen:
  def main(): Unit =
    val lines                = readLines("input10")
    given array: Array[Char] = lines.mkString.toCharArray
    given len: Int           = lines.head.length
    val trailheads           = array.zipWithIndex.collect { case (c, index) if c == '0' => Pos.fromIndex(index) }
    val trailResults         = trailheads.map(trail(_))
    // Part 1
    println(s"Part 1: ${trailResults.map(_.distinct.size).sum}") // 776
    // Part 2
    println(s"Part 2: ${trailResults.map(_.size).sum}") // 1657

  private def trail(pos: Pos, prev: Char = ('0' - 1).toChar)(using array: Array[Char], len: Int): List[Pos] = getChar(pos) match
    case None                     => Nil
    case Some(c) if c - prev != 1 => Nil
    case Some('9')                => List(pos)
    case Some(c)                  => Directions.map(dir => trail(pos + dir, c)).reduce(_ ++ _)
