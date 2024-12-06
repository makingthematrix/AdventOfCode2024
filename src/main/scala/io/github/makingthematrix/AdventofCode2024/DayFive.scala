package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines

object DayFive:
  private def sumMiddles(updates: Seq[Array[Int]]): Int = updates.map(update => update(update.length / 2)).sum

  def main(): Unit =
    val lines       = readLines("input5")
    val rules       = lines.takeWhile(_.nonEmpty).collect { case s"$a|$b" => (a.toInt, b.toInt) }
    val ruleSet     = rules.toSet
    val updates     = lines.drop(rules.length + 1).map(_.split(",").map(_.toInt))
    val (good, bad) = updates.partition(update => update.view.zip(update.tail).forall(ruleSet))
    println(s"Part 1: ${sumMiddles(good)}") // 4872
    val fixed       = bad.map(_.sortWith((a, b) => ruleSet((a, b))))
    println(s"Part 2: ${sumMiddles(fixed)}") // 5564
