package io.github.makingthematrix.AdventofCode2024

import scala.language.experimental.namedTuples
import io.github.makingthematrix.{Dir, Pos, isValidPosition, readLines, toPos, add}

object DayEight:
  private def findAntennas(frequency: Char)(using array: Array[Char], len: Int): Array[Pos] =
    array.zipWithIndex.collect { case (c, i) if c == frequency => toPos(i) }

  private inline def getVector(a: Pos, b: Pos): Dir = (b.x - a.x, b.y - a.y)

  private def findAntinode(antenna: Pos, vector: Dir)(using len: Int): Option[Pos] =
    val an = add(antenna, vector)
    if isValidPosition(an) then Some(an) else None

  private def findAntinodes(antenna: Pos, vector: Dir)(using len: Int): Vector[Pos] =
    Vector.unfold(0) { n =>
      val an = add(antenna, vector, n)
      if isValidPosition(an) then Some((an, n + 1)) else None
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
