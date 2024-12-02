package io.github.makingthematrix.AdventofCode2024

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import math.{abs, signum}

object DayTwo {
  private val path = Path.of("resources/input2")

  private def readInput: Seq[Seq[Int]] =
    Files.readAllLines(path).asScala.toSeq.map {
      _.split(" ").toSeq.map(n => n.trim.toInt)
    }

  private def findDir(seq: Seq[Int]): Option[Int] =
    if (seq.length < 2) None
    else if(seq.length == 3 && seq(0) != seq(1)) Some(signum(seq(1) - seq(0)))
    else {
      val dirs = (0 to 2).map(i => signum(seq(i + 1) - seq(i)))
      if (dirs.count(_ == 1)>= 2) Some(1)
      else if (dirs.count(_ == -1)>= 2) Some(-1)
      else None
    }

  private def isSafe(seq: Seq[Int], useDampener: Boolean = false): Boolean = findDir(seq) match {
    case None => false
    case Some(dir) =>
      val errors = seq.zip(seq.tail).zipWithIndex.collect {
        case ((a, b), i) if dir == 1 && a >= b  => i
        case ((a, b), i) if dir == -1 && a <= b => i
        case ((a, b), i) if abs(b - a) > 3      => i
      }
      if (errors.isEmpty) true
      else if (!useDampener) false
      else
        errors.exists { e =>
          isSafe(seq.take(e) ++ seq.drop(e + 1)) ||
            isSafe(seq.take(e + 1) ++ seq.drop(e + 2))
        }
  }

  @main def main(): Unit = {
    val seqs = readInput
    // Part 1
    val res1 = seqs.count(isSafe(_, false))
    println(res1) // 220

    // Part 2
    val res2 = seqs.count(isSafe(_, true))
    println(res2) // 296
  }
}
