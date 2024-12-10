package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.readString
import scala.annotation.tailrec
import scala.collection.mutable

object DayNine:
  @main def main(): Unit =
    val disk = readString("input9").toCharArray.map(_.toInt - 48)
    val skipFirst = disk.head
    val expanded = expand(disk.tail)
    // Part 1
    val defragmented = defragment1(expanded)
    val res1 = sumUp1(defragmented, skipFirst)
    println(s"Part 1: $res1") // 6241633730082
    // Part 2
    val fragments = defragment2(expanded)
    val res2 = sumUp2(fragments, skipFirst)
    println(s"Part 2: $res2") // 6265268809555

  private def expand(disk: Array[Int]): Array[Int] = {
    val builder = new mutable.ArrayBuffer[Int]()
    var freeOrFile = true
    var index = 1
    for n <- disk do
      if freeOrFile then
        builder.appendAll(Seq.fill(n)(0))
      else
        builder.appendAll(Seq.fill(n)(index))
        index += 1
      freeOrFile = !freeOrFile
    builder.toArray
  }

  // Part 1

  private def defragment1(expanded: Array[Int]): Array[Int] =
    val clone = expanded.clone()
    var i = 0
    var j = clone.lastIndexWhere(_ > 0)
    while i < j do
      if clone(i) == 0 then
        clone.update(i, clone(j))
        clone.update(j, 0)
        j -= 1
        while clone(j) == 0 do j -= 1
      i += 1
    clone

  private def sumUp1(expanded: Array[Int], skipFirst: Int): Long =
    expanded.zipWithIndex.foldLeft(0L) { case (sum, (n,index)) => sum + n.toLong * (index + skipFirst) }

  // Part 2

  private final case class Fragment(n: Int, length: Int):
    inline def isFree: Boolean = n == 0
    def sum(fromIndex: Long): Long = (0 until length).map(i => (fromIndex + i.toLong) * n.toLong).sum

  private def defragment2(expanded: Array[Int]): Array[Fragment] =
    val fragments = expanded.foldLeft(List.empty[Fragment]) {
      case (head :: tail, n) if n == head.n => Fragment(n, head.length + 1) :: tail
      case (acc, n) => Fragment(n, 1) :: acc
    }.reverse.toArray
    defrag(fragments)

  private def sumUp2(fragments: Array[Fragment], skipFirst: Int): Long =
    fragments.foldLeft((0L, skipFirst.toLong)) {
      case ((sum, index), fragment) => (sum + fragment.sum(index), index + fragment.length)
    }._1

  private def findFirstFree(array: Array[Fragment], from: Int): Option[(Int, Int)] =
    (from until array.length)
      .view
      .map(i => (i, array(i)))
      .collectFirst { case (i, f) if f.isFree => (i, f.length) }

  private def findLastMatching(array: Array[Fragment], from: Int, maxLength: Int): Option[(Int, Fragment)] =
    (from until array.length)
      .view
      .map(j => (j, array(j)))
      .findLast((_, f) => !f.isFree && f.length <= maxLength)

  private def swapInPlace(array: Array[Fragment], fragment: Fragment, i: Int, j: Int): Array[Fragment] =
    array.update(i, fragment)
    array.update(j, Fragment(0, fragment.length))
    array

  private def swap(array: Array[Fragment], fragment: Fragment, i: Int, j: Int, freeLength: Int): Array[Fragment] =
    val ab = new mutable.ArrayBuffer[Fragment]()
    ab.addAll(array.slice(0, i))
    ab.addOne(fragment)
    ab.addOne(Fragment(0, freeLength - fragment.length))
    ab.addAll(array.slice(i + 1, j))
    ab.addOne(Fragment(0, fragment.length))
    ab.addAll(array.slice(j + 1, array.length))
    ab.toArray

  @tailrec private def defrag(array: Array[Fragment], startIndex: Int = 0): Array[Fragment] =
    findFirstFree(array, startIndex) match
      case None => array
      case Some(i, freeLength) =>
        findLastMatching(array, i, freeLength) match
          case None => defrag(array, i + 1)
          case Some(j, fragment) =>
            val newArray =
              if fragment.length == freeLength then swapInPlace(array, fragment, i, j)
              else swap(array, fragment, i, j, freeLength)
            defrag(newArray, i + 1)
