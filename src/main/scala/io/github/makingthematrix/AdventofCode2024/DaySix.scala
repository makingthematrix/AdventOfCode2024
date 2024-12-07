package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.{getChar, readLines}
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

object DaySix:
  @tailrec private def getPath(x: Int, y: Int, dir: (Int, Int) = (-1, 0), uniques: Set[(Int, Int)] = Set.empty)
                              (using arr: Array[Char], len: Int): Set[(Int, Int)] =
    val updated = if getChar(x, y).contains('.') then uniques + ((x, y)) else uniques
    getChar(x + dir._1, y + dir._2) match
      case None      => updated
      case Some('.') => getPath(x + dir._1, y + dir._2, dir, updated)
      case Some('#') => getPath(x, y, (dir._2, -dir._1), updated)

  private val Marks = Map('#' -> '1', '1' -> '2', '2' -> '3', '3' -> '4')

  @tailrec private def isInfiniteLoop(x: Int, y: Int, dir: (Int, Int) = (-1, 0))
                                     (using arr: Array[Char], len: Int): Boolean =
    getChar(x + dir._1, y + dir._2) match
      case None      => false
      case Some('.') => isInfiniteLoop(x + dir._1, y + dir._2, dir)
      case Some('4') => true
      case Some(c)   => arr.update((x + dir._1) * len + y + dir._2, Marks(c)); isInfiniteLoop(x, y, (dir._2, -dir._1))

  @main def main(): Unit =
    val (array, len) = readLines("input6") match { case lines => (lines.mkString.toCharArray, lines.head.length) }
    val start        = array.indexOf('^')
    val t@(x, y)     = (start / len, start % len)
    array.update(start, '.')
    // Part 1
    val path = getPath(x, y)(using array, len)
    println(s"Part 1: ${path.size}") // 5329
    // Part 2
    val res2 = (path - t).toArray.par
      .map { (a, b) => array.updated(a * len + b, '#') }
      .count { arr => isInfiniteLoop(x, y)(using arr, len) }
    println(s"Part 2: $res2") // 2162
