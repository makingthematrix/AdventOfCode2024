package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines
import math.{abs, signum}

object DayTwo:
  private def isSafe(arr: Array[Int], useDampener: Boolean = false): Boolean =
    val dir   = signum(arr(1) - arr(0))
    val check = arr.zip(arr.tail).forall((a, b) => a != b && signum(b - a) == dir && abs(b - a) <= 3)
    if check || !useDampener then check
    else arr.view.indices.map(i => arr.take(i) ++ arr.drop(i + 1)).exists(isSafe(_))

  def main(): Unit =
    val seqs = readLines("input2").map(_.split(" ").map(_.toInt))
    println(s"Part 1: ${seqs.count(isSafe(_))}") // 220
    println(s"Part 2: ${seqs.count(isSafe(_, true))}") // 296
