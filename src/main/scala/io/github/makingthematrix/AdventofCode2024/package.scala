package io.github.makingthematrix

package object AdventofCode2024

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

inline def readLines(fileName: String): Seq[String] = Files.readAllLines(Path.of(s"resources/$fileName")).asScala.toSeq
inline def readString(fileName: String): String = Files.readString(Path.of(s"resources/$fileName"))

final case class Pos(x: Int, y: Int):
  inline def toChar(using arr: Array[Char], len: Int): Option[Char] = getChar(x, y)
  inline def isValid(using len: Int): Boolean = x >= 0 && x < len && y >= 0 && y < len
  inline def toIndex(using len: Int): Int = x * len + y
  inline def +(dir: Dir): Pos = Pos(x + dir.x, y + dir.y)

object Pos:
  inline def fromIndex(index: Int)(using len: Int): Pos = new Pos(index / len, index % len)

final case class  Dir(x: Int, y: Int):
  inline def *(n: Int): Dir = Dir(n * x, n * y)
  inline def turnRight: Dir = Dir(y, -x)

inline def getChar(x: Int, y: Int)(using arr: Array[Char], len: Int): Option[Char] =
  if x < 0 || y < 0 || x >= len || y >= len then None else Some(arr(x * len + y))

def printBoard(using arr: Array[Char], len: Int): Unit = arr.grouped(len).foreach(line => println(line.mkString))
