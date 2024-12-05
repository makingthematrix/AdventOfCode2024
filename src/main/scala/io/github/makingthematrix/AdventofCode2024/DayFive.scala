package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines
import scala.annotation.tailrec

object DayFive:
  @tailrec private def fixUpdate(update: Array[Int], n: Int = 0)(using ruleSet: Set[(Int, Int)]): Array[Int] =
    if n == update.length - 1 then update
    else if ruleSet((update(n), update(n + 1))) then fixUpdate(update, n + 1)
    else
      val t = update(n); update(n) = update(n + 1); update(n + 1) = t // not very safe, but works...
      fixUpdate(update, if n > 0 then n - 1 else 1)

  private def sumMiddles(updates: Seq[Array[Int]]): Int = updates.map(update => update(update.length / 2)).sum

  def main(): Unit =
    val lines                      = readLines("input5")
    val rules                      = lines.takeWhile(_.nonEmpty).map(_.split("\\|")).collect { case Array(a, b) => (a.toInt, b.toInt) }
    given ruleSet: Set[(Int, Int)] = rules.toSet
    val updates                    = lines.drop(rules.length + 1).map(_.split(",").map(_.toInt))
    val (correct, incorrect)       = updates.partition(update => update.view.zip(update.tail).forall(ruleSet))
    println(s"Part 1: ${sumMiddles(correct)}") // 4872
    println(s"Part 2: ${sumMiddles(incorrect.map(fixUpdate(_)))}") // 5564
