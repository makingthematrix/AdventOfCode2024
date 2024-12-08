package io.github.makingthematrix

package object AdventofCode2024

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

inline def readLines(fileName: String): Seq[String] = Files.readAllLines(Path.of(s"resources/$fileName")).asScala.toSeq
inline def readString(fileName: String): String = Files.readString(Path.of(s"resources/$fileName"))

inline def getChar(x: Int, y: Int)(using arr: Array[Char], len: Int): Option[Char] =
  if x < 0 || y < 0 || x >= len || y >= len then None else Some(arr(x * len + y))

def printBoard(using arr: Array[Char], len: Int): Unit =
  arr.grouped(len).foreach(line => println(line.mkString))
