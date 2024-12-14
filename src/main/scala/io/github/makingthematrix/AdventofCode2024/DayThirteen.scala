package io.github.makingthematrix.AdventofCode2024

import io.github.makingthematrix.{Dir, Pos, readLines}

object DayThirteen {
  private val MaxIterations = 100
  case class Data(ax: Int, bx: Int, ay: Int, by: Int, px: Int, py: Int) {
    private inline def isZero(x: Double): Boolean = x < 0.001 && x > -0.001

    lazy val tokens: Int =
      val dira = Dir(ax, ay)
      val dirb = Dir(bx, by)
      val posp = Pos(px, py)
      val combinations =
        (for {
          a <- (0 to MaxIterations).reverse if a * ax + MaxIterations * bx >= px && a * ay + MaxIterations * by >= py
          b <- (0 to MaxIterations).reverse if b * bx + a * ax >= px && b * by + a * ay >= py
        } yield (a, b)).view
      val result = combinations.find { (a, b) =>
        val aa = posp - (dira * a)
        val bb = (dirb * b)
        aa.x == bb.x && aa.y == bb.y
      }
      result.map((a,b) => a * 3 + b).getOrElse(0)
      //val results = combinations.filter((a, b) => a * ax + b * bx == px && a * ay + b * by == py).map((a, b) => a * 3 + b)
      //if results.nonEmpty then results.min else 0
  }

  private def parse(lines: Seq[String]): Seq[Data] =
    lines.grouped(3).map { triple =>
      val (ax, ay) = triple(0) match { case s"Button A: X+$x, Y+$y" => (x.toInt, y.toInt) }
      val (bx, by) = triple(1) match { case s"Button B: X+$x, Y+$y" => (x.toInt, y.toInt) }
      val (px, py) = triple(2) match { case s"Prize: X=$x, Y=$y" => (x.toInt, y.toInt) }
      Data(ax = ax, bx = bx, ay = ay, by = by, px = px, py = py)
    }.toSeq

  @main def main(): Unit = {
    val lines = readLines("input13").map(_.trim).filter(_.nonEmpty)
    val data = parse(lines)

    val res1 = data.map(_.tokens).sum
    println(s"Part 1: $res1") // 26005
  }
}
/*
```
Button A: X+71, Y+36
Button B: X+28, Y+62
Prize: X=11070, Y=4082
```
a * 71 + b * 28 = 11070
a * 36 + b * 62 = 4082
AX=71, AY=36, BX=28, BY=62, PX=11070, PY=4082
---
b = (4082 - 36a) / 62
71a + 28/62 * (4082 - 36a) = 11070
71a + 28/62 * 4082 - 28/62*36a = 11070
a(71 - 28/62*36) + 28/62 * 4082 = 11070
a = (11070 - 28/62 * 4082) / (71 - 28/62*36)
---
a = (PX - (BX/BY * PY)) / (AX - (BX/BY*AY))
b = (PY - (AY*a)) / BY
---
Both a and b need to be integers.
They both need to be lower than 100.

*/

