package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines
import scala.annotation.tailrec

object DayFive:
  @main def main(): Unit =
    val lines = readLines("input5_test")
    val rules = lines.takeWhile(_.nonEmpty).map { line =>
      val pair = line.split("\\|")
      (pair(1).toInt, pair(0).toInt) // the swap here is on purpose
    }
    val updates = lines.drop(rules.length + 1).map(_.split(",").map(_.toInt).toSeq)

    val ruleSet = rules.toSet
    val (correct, incorrect) = updates.partition {
      update => update.view.zip(update.tail).forall { (a, b) => ruleSet.contains((b, a)) }
    }

    // Part 1
    val res1 = correct.map(update => update(update.length / 2)).sum
    println(res1) // 4872

    // Part 2
    @tailrec def swap(update: Seq[Int], n: Int): Seq[Int] =
      if (n == update.length - 1) update
      else if (!ruleSet.contains(update(n), update(n + 1))) swap(update, n + 1)
      else swap(update.take(n) ++ Seq(update(n + 1), update(n)) ++ update.drop(n + 2), if (n > 0) n - 1 else 1)

    val res2 = incorrect.map(swap(_, 0)).map(update => update(update.length / 2)).sum
    println(res2) // 5564
