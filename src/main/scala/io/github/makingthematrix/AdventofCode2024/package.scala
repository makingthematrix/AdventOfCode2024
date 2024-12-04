package io.github.makingthematrix

package object AdventofCode2024

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

inline def readLines(fileName: String): Seq[String] = Files.readAllLines(Path.of(s"resources/$fileName")).asScala.toSeq
inline def readString(fileName: String): String = Files.readString(Path.of(s"resources/$fileName"))