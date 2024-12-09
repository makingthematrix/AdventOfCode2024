package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines

object DayEight:
  private def findAntennas(frequency: Char)(using array: Array[Char], len: Int): Array[(Int, Int)] =
    array.zipWithIndex.collect { case (c, i) if c == frequency => (i / len, i % len) }

  private def getVector(a: (Int, Int), b: (Int, Int)): (Int, Int) = (b._1 - a._1, b._2 - a._2)

  private def findAntinode(antenna: (Int, Int), vector: (Int, Int))(using len: Int): Option[(Int, Int)] =
    val an = (antenna._1 + vector._1, antenna._2 + vector._2)
    if (an._1 >= 0 && an._1 < len && an._2 >= 0 && an._2 < len) Some(an) else None

  private def findAntinodes(antenna: (Int, Int), vector: (Int, Int))(using len: Int): Vector[(Int, Int)] =
    Vector.unfold(0) { n =>
      val an = (antenna._1 + n * vector._1, antenna._2 + n * vector._2)
      if (an._1 >= 0 && an._1 < len && an._2 >= 0 && an._2 < len) Some((an, n + 1)) else None
    }

  @main def main(): Unit =
    val lines                = readLines("input8")
    given array: Array[Char] = lines.mkString.toCharArray
    given len: Int           = lines.head.length
    val frequencies          = array.toSet - '.'
    val allAntennas          = frequencies.toSeq.map(findAntennas)
    val allPairs             = allAntennas.map { _.combinations(2).map(seq => (seq(0), seq(1))).toSeq }
    val antennasWithVectors =
      for
        pairs             <- allPairs
        (a, b)            <- pairs
        (antenna, vector) <- Seq(b -> getVector(a, b), a -> getVector(b, a))
      yield (antenna, vector)
    // Part 1
    val res1 = for { (a, v) <- antennasWithVectors; node <- findAntinode(a, v) } yield node
    println(s"Part 1: ${res1.distinct.size}") // 271
    //Part 2
    val res2 = for { (a, v) <- antennasWithVectors; nodes <- findAntinodes(a, v) } yield nodes
    println(s"Part 2: ${res2.distinct.size}") // 994
