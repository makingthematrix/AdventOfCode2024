package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines
import math.{abs, signum}

object DayTwo:
  private def isSafe(arr: Array[Int], useDampener: Boolean): Boolean =
    val dir   = signum(arr(1) - arr(0))
    val check = arr.zip(arr.tail).forall((a, b) => a != b && signum(b - a) == dir && abs(b - a) <= 3)
    if check || !useDampener then check
    else arr.indices.view.map(i => arr.take(i) ++ arr.drop(i + 1)).exists(isSafe(_, false))

  def main(): Unit =
    val seqs = readLines("input2").map(_.split(" ").map(_.toInt))
    println(s"Part 1: ${seqs.count(isSafe(_, false))}") // 220
    println(s"Part 2: ${seqs.count(isSafe(_, true))}") // 296
