package io.github.makingthematrix

package object AdventofCode2024

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.language.experimental.namedTuples

type Pos = (x: Int, y: Int)
type Dir = (x: Int, y: Int)

inline def readLines(fileName: String): Seq[String] = Files.readAllLines(Path.of(s"resources/$fileName")).asScala.toSeq
inline def readString(fileName: String): String = Files.readString(Path.of(s"resources/$fileName"))

inline def getChar(x: Int, y: Int)(using arr: Array[Char], len: Int): Option[Char] =
  if x < 0 || y < 0 || x >= len || y >= len then None else Some(arr(x * len + y))

inline def getChar(pos: Pos)(using arr: Array[Char], len: Int): Option[Char] = getChar(pos.x, pos.y)

inline def isValidPosition(pos: Pos)(using len: Int): Boolean = pos.x >= 0 && pos.x < len && pos.y >= 0 && pos.y < len
inline def add(pos: Pos, dir: Dir, n: Int = 1): Pos = (x = pos.x + n * dir.x, y = pos.y + n * dir.y)
inline def toIndex(pos: Pos)(using len: Int): Int = pos.x * len + pos.y
inline def toPos(index: Int)(using len: Int): Pos = (x = index / len, y = index % len)

def printBoard(using arr: Array[Char], len: Int): Unit = arr.grouped(len).foreach(line => println(line.mkString))


