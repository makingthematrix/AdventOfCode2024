package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readLines
import scala.collection.parallel.CollectionConverters.*

type Op = (Long, Long) => Long

final case class Line(testValue: Long, numbers: Array[Long]):
  private def applyOps(ops: List[Op]): Long =
    numbers.tail.zip(ops).foldLeft(numbers.head) {
      case (a, (b, op)) => op(a, b)
    }

  def isCalibrated(ops: LazyList[Vector[List[Op]]]): Boolean =
    ops(numbers.length - 1).exists(applyOps(_) == testValue)

object DaySeven:
  private def lazyListLoop(ops: List[Op], n: Int = 0): LazyList[Vector[List[Op]]] =
    val res =
      (0 until n).foldLeft(Vector(List.empty[Op])) {
        (lst, _) => lst.flatMap(l => ops.map(_ :: l))
      }
    res #:: lazyListLoop(ops, n + 1)

  private def sumCalibrated(lines: Seq[Line], ops: List[Op]): Long =
    val allOps = lazyListLoop(ops)
    lines.par.collect {
      case line if line.isCalibrated(allOps) => line.testValue
    }.sum

  @main def main(): Unit =
    val lines = readLines("input7").collect {
      case s"$value: $numbers" => Line(value.toLong, numbers.split(' ').map(_.toLong))
    }
    // Part 1
    val res1 = sumCalibrated(lines, List(_ + _, _ * _))
    println(s"Part 1: $res1") // 4555081946288
    // Part 2
    val res2 = sumCalibrated(lines, List(_ + _, _ * _, (a, b) => s"$a$b".toLong))
    println(s"Part 2: $res2") // 227921760109726
