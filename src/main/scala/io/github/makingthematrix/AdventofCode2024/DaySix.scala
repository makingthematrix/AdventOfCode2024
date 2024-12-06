package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines
import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector

object DaySix:
  private def getChar(x: Int, y: Int)(using array: Array[Char], len: Int): Option[Char] =
    if x < 0 || y < 0 || x >= len || y >= len then None else Some(array(x * len + y))

  @tailrec private def traverse1(x: Int, y: Int, dir: (Int, Int) = (-1, 0), uniques: Int = 0)(using array: Array[Char], len: Int): Int =
    val newUniques = getChar(x, y) match
      case Some('.') => array.update(x * len + y, 'X'); uniques + 1
      case _         => uniques
    getChar(x + dir._1, y + dir._2) match
      case None                  => newUniques
      case Some('.') | Some('X') => traverse1(x + dir._1, y + dir._2, dir, newUniques)
      case Some('#')             => traverse1(x, y, (dir._2, -dir._1), newUniques)

  private val Marks = Map('#' -> '1', '1' -> '2', '2' -> '3', '3' -> '4')

  @tailrec private def traverse2(x: Int, y: Int, dir: (Int, Int) = (-1, 0))(using array: Array[Char], len: Int): Boolean =
    getChar(x + dir._1, y + dir._2) match
      case None      => false
      case Some('.') => traverse2(x + dir._1, y + dir._2, dir)
      case Some('4') => true
      case Some(c)   => array.update((x + dir._1) * len + y + dir._2, Marks(c)); traverse2(x, y, (dir._2, -dir._1))

  @main def main(): Unit =
    val (array, len) = readLines("input6") match { case lines => (lines.mkString.toCharArray, lines.head.length) }
    val start        = array.indexOf('^')
    val (x, y)       = (start / len, start % len)
    array.update(start, '.')
    // Part 1
    val res1 = traverse1(x, y)(using array.clone(), len)
    println(s"Part 1: $res1") // 5329
    // Part 2
    val res2 =
      ParVector.fromSpecific(0 until array.length)
        .collect { case i if i != start && array(i) == '.' => array.updated(i, '#') }
        .count { arr => traverse2(x, y)(using arr, len) }
    println(s"Part 2: $res2") // 2162
