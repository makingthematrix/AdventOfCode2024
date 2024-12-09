package io.github.makingthematrix.AdventofCode2024

import scala.language.experimental.namedTuples
import io.github.makingthematrix.{getChar, readLines, Pos, Dir, add, toIndex, toPos}
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

object DaySix:
  @tailrec private def getPath(pos: Pos, dir: Dir = (-1, 0), uniques: Set[Pos] = Set.empty)
                              (using arr: Array[Char], len: Int): Set[Pos] =
    val updated = if getChar(pos).contains('.') then uniques + pos else uniques
    getChar(add(pos, dir)) match
      case None      => updated
      case Some('.') => getPath(add(pos, dir), dir, updated)
      case Some(_)   => getPath(pos, (x = dir.y, y = -dir.x), updated)

  private val Marks = Map('#' -> '1', '1' -> '2', '2' -> '3', '3' -> '4')

  @tailrec private def isInfiniteLoop(pos: Pos, dir: Dir = (-1, 0))
                                     (using arr: Array[Char], len: Int): Boolean =
    val newPos = add(pos, dir)
    getChar(newPos) match
      case None      => false
      case Some('.') => isInfiniteLoop(newPos, dir)
      case Some('4') => true
      case Some(c)   =>
        arr.update(toIndex(add(pos, dir)), Marks(c))
        isInfiniteLoop(pos, (x = dir.y, y = -dir.x))

  def main(): Unit =
    val lines = readLines("input6")
    given array: Array[Char] = lines.mkString.toCharArray
    given len: Int           = lines.head.length
    val startIndex           = array.indexOf('^')
    val startPos             = toPos(startIndex)
    array.update(startIndex, '.')
    // Part 1
    val path = getPath(startPos)
    println(s"Part 1: ${path.size}") // 5329
    // Part 2
    val res2 = (path - startPos).toArray.par
      .map { pos => array.updated(toIndex(pos), '#') }
      .count { arr => isInfiniteLoop(startPos)(using arr, len) }
    println(s"Part 2: $res2") // 2162
