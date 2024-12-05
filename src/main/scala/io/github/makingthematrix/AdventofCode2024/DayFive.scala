package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines
import scala.annotation.tailrec

object DayFive:
  @main def main(): Unit =
    val lines = readLines("input5")
    val rules = lines.takeWhile(_.nonEmpty).map { line =>
      val pair = line.split("\\|")
      (pair(0).toInt, pair(1).toInt)
    }

    val ruleSet              = rules.toSet
    val updates              = lines.drop(rules.length + 1).map(_.split(",").map(_.toInt))
    val (correct, incorrect) = updates.partition(update => update.view.zip(update.tail).forall(ruleSet(_)))

    // Part 1
    val res1 = correct.map(update => update(update.length / 2)).sum
    println(res1) // 4872

    // Part 2
    @tailrec def swap(update: Array[Int], n: Int = 0): Array[Int] =
      if n == update.length - 1 then update
      else if ruleSet((update(n), update(n + 1))) then swap(update, n + 1)
      else swap(update.take(n) ++ Array(update(n + 1), update(n)) ++ update.drop(n + 2), if n > 0 then n - 1 else 1)

    val res2 = incorrect.map(swap(_)).map(update => update(update.length / 2)).sum
    println(res2) // 5564
