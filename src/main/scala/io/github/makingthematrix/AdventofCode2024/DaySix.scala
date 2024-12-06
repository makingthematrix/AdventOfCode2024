package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines
import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector

object DaySix:
  private def getChar(x: Int, y: Int)(using arr: Array[Char], len: Int): Option[Char] =
    if x < 0 || y < 0 || x >= len || y >= len then None else Some(arr(x * len + y))

  @tailrec private def getPath(x: Int, y: Int, dir: (Int, Int) = (-1, 0), lst: List[(Int, Int)] = Nil)(using arr: Array[Char], len: Int): List[(Int, Int)] =
    val updated = getChar(x, y) match
      case Some('.') => arr.update(x * len + y, 'X'); (x, y) :: lst
      case _         => lst
    getChar(x + dir._1, y + dir._2) match
      case None                  => updated
      case Some('.') | Some('X') => getPath(x + dir._1, y + dir._2, dir, updated)
      case Some('#')             => getPath(x, y, (dir._2, -dir._1), updated)

  private val Marks = Map('#' -> '1', '1' -> '2', '2' -> '3', '3' -> '4')

  @tailrec private def isInfiniteLoop(x: Int, y: Int, dir: (Int, Int) = (-1, 0))(using arr: Array[Char], len: Int): Boolean =
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
    val path = getPath(x, y)(using array.clone(), len)
    println(s"Part 1: ${path.length}") // 5329
    // Part 2
    val res2 = ParVector.fromSpecific(path.toSet -- Set(t))
      .map { (a, b) => array.updated(a * len + b, '#') }
      .count { arr => isInfiniteLoop(x, y)(using arr, len) }
    println(s"Part 2: $res2") // 2162
