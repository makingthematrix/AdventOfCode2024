package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines

object DayEight:
  private def findAntennas(frequency: Char, array: Array[Char], len: Int): Array[(Int, Int)] =
    array.zipWithIndex.collect { case (c, i) if c == frequency => (i / len, i % len) }

  private def getVector(a: (Int, Int), b: (Int, Int)): (Int, Int) = (b._1 - a._1, b._2 - a._2)

  private def findAntinode(antenna: (Int, Int), vector: (Int, Int), len: Int): Option[(Int, Int)] =
    val an = (antenna._1 + vector._1, antenna._2 + vector._2)
    if (an._1 >= 0 && an._1 < len && an._2 >= 0 && an._2 < len) Some(an) else None

  private def findAntinodes(antenna: (Int, Int), vector: (Int, Int), len: Int): Vector[(Int, Int)] =
    Vector.unfold(0) { n =>
      val an = (antenna._1 + n * vector._1, antenna._2 + n * vector._2)
      if (an._1 >= 0 && an._1 < len && an._2 >= 0 && an._2 < len) Some((an, n + 1)) else None
    }

  @main def main(): Unit =
    val (array, len) = readLines("input8") match { case lines => (lines.mkString.toCharArray, lines.head.length) }
    val frequencies  = array.toSet - '.'
    val allAntennas  = frequencies.toSeq.map { findAntennas(_, array, len) }
    val allPairs     = allAntennas.map { _.combinations(2).map(seq => (seq(0), seq(1))).toSeq }
    val allVectors   = allPairs.map { _.flatMap((a, b) => Seq(b -> getVector(a, b), a -> getVector(b, a))) }
    // Part 1
    val res1 = for { vectors <- allVectors; (a, v) <- vectors; node <- findAntinode(a, v, len) } yield node
    println(s"Part 1: ${res1.distinct.size}") // 271
    //Part 2
    val res2 = for { vectors <- allVectors; (a, v) <- vectors; nodes <- findAntinodes(a, v, len) } yield nodes
    println(s"Part 2: ${res2.distinct.size}") // 994
