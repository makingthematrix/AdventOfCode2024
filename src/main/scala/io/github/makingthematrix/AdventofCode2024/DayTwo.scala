package io.github.makingthematrix.AdventofCode2024

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import math.{abs, signum}

object DayTwo:
  private def isSafe(seq:Seq[Int], useDampener: Boolean): Boolean =
    val dir   = signum(seq(1) - seq(0))
    val check = seq.zip(seq.tail).forall((a, b) => a != b && signum(b - a) == dir && abs(b - a) <= 3)
    if check || !useDampener then check
    else seq.indices.view.map(i => seq.take(i) ++ seq.drop(i + 1)).exists(isSafe(_, false))

  def main(): Unit =
    val seqs = Files.readAllLines(Path.of("resources/input2")).asScala.toSeq.map { _.split(" ").toSeq.map(n => n.trim.toInt) }
    println(s"Part 1: ${seqs.count(isSafe(_, false))}") // 220
    println(s"Part 2: ${seqs.count(isSafe(_, true))}") // 296
