package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.{Dir, Directions, Pos, getChar, readLines}
import scala.collection.mutable.ArrayBuffer

object DayTwelve:
  @main def main(): Unit =
    val lines = readLines("input12")
    given array: Array[Char] = lines.mkString.toCharArray
    given len: Int = lines.head.length
    val regions = calculateRegions
    // Part 1
    val res1 = regions.map { oneRegionArray =>
      oneRegionArray.count(_ == '#') * calculatePerimeter(using oneRegionArray, len)
    }.sum
    println(s"Part 1: $res1") // 1304764
    // Part 2
    val res2 = regions.map { oneRegionArray =>
      oneRegionArray.count(_ == '#') * calculateSides(using oneRegionArray, len)
    }.sum
    println(s"Part 2: $res2") // 811148

  private def markRegion(from: Char)(using array: Array[Char], len: Int): Array[Char] =
    def go(pos: Pos, newArray: Array[Char] = Array.fill(array.length)('.')): Array[Char] =
      if (getChar(pos).contains(from))
        newArray.update(pos.toIndex, '#')
        array.update(pos.toIndex, '.')
        Directions.foreach(dir => go(pos + dir, newArray))
      newArray
    go(Pos.fromIndex(array.indexWhere(_ == from)))

  private def calculateRegions(using array: Array[Char], len: Int): Seq[Array[Char]] =
    val buf = ArrayBuffer[Array[Char]]()
    while array.exists(_ != '.') do
      buf.addOne(markRegion(array.find(_ != '.').get))
    buf.toSeq

  private def calculatePerimeter(using array: Array[Char], len: Int): Int =
    array
      .zipWithIndex
      .collect { case ('#', i) => Pos.fromIndex(i) }
      .map { pos => Directions.count { dir => !getChar(pos + dir).contains('#') } }.sum

  private val dirs = Seq(Dir(0, 0), Dir(0, 1), Dir(1, 0), Dir(1, 1))
  private val diagonals = Seq(Seq(Dir(0, 0), Dir(1, 1)), Seq(Dir(0, 1), Dir(1, 0)))

  private def calculateSides(using array: Array[Char], len: Int): Int =
    (for
       x <- -1 until len
       y <- -1 until len
       pos = Pos(x, y)
       count = dirs.count { dir => getChar(pos + dir).contains('#') }
     yield
       if count == 0 || count == 4 then 0
       else if count == 1 || count == 3 then 1
       else if diagonals.exists(_.forall(dir => getChar(pos + dir).contains('#'))) then 2
       else 0
    ).sum
