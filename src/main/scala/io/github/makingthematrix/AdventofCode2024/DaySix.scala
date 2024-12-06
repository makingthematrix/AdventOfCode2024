package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines
import scala.annotation.tailrec

object DaySix:
  private val Marks = Map('#' -> '1', '1' -> '2', '2' -> '3', '3' -> '4')

  private def getChar(x: Int, y: Int)(using array: Array[Char], len: Int): Option[Char] =
    if x < 0 || y < 0 || x >= len || y >= len then None else Some(array(x * len + y))

  @tailrec private def traverse(x: Int, y: Int, dir: (Int, Int) = (-1, 0), uniques: Int = 0)(using array: Array[Char], len: Int): Option[Int] =
    def mark(c: Char): Unit = array.update((x + dir._1) * len + y + dir._2, Marks(c))

    val newUniques = getChar(x, y) match
      case Some('.') => array.update(x * len + y, 'X'); uniques + 1
      case _         => uniques

    getChar(x + dir._1, y + dir._2) match
      case None                  => Some(newUniques)
      case Some('.') | Some('X') => traverse(x + dir._1, y + dir._2, dir, newUniques)
      case Some('4')             => None
      case Some(c)               => mark(c); traverse(x, y, (dir._2, -dir._1), newUniques)

  @main def main(): Unit =
    val (array, len) = readLines("input6") match { case lines => (lines.mkString.toCharArray, lines.head.length) }
    val start        = array.indexOf('^')
    val startPos     = (start / len, start % len)
    array.update(start, '.')

    val res1 = traverse(startPos._1, startPos._2)(using array.clone(), len).getOrElse(0)
    println(res1) // 5329

    val res2 =
      (0 until array.length)
        .view
        .collect { case i if i != start && array(i) == '.' => array.updated(i, '#') }
        .count { arr => traverse(startPos._1, startPos._2)(using arr, len).isEmpty }
    println(res2) // 2162
