package org.tkareine.adventofcode2022.day04

import java.nio.file.Path
import scala.io.Source
import scala.util.Using

extension (r1: Range)
  /** Caveat: assumes that both ranges have the same step value. */
  def fullOverlap(r2: Range): Boolean =
    r1.step == r2.step && r1.contains(r2.start) && r1.contains(r2.end)

  /** Caveat: assumes that both ranges have the same step value. */
  def overlaps(r2: Range): Boolean =
    r1.step == r2.step &&
      ((r2.start <= r1.last && r2.last >= r1.start) || // r2 starts before r1 ends AND r2 ends after r1 starts
        (r2.last >= r1.start && r2.start <= r1.last)) // r2 ends after r1 starts AND r2 starts before r1 ends

object Range {
  def decode(s: String): Range = {
    val limits = s.split("-", 2)
    limits(0).toInt to limits(1).toInt
  }
}

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day04.txt")

@main def main(): Unit = {
  Using(Source.fromFile(InputFile.toFile)) { file =>
    def booleanToInt(b: Boolean): Int =
      if b then 1 else 0

    val (fullOverlaps, partialOverlaps) = file
      .getLines()
      .map { line =>
        val assignments = line.split(",", 2)
        val (r1, r2) = Range.decode(assignments(0)) -> Range.decode(assignments(1))
        val isFullOverlap = r1.fullOverlap(r2) || r2.fullOverlap(r1)
        val isPartialOverlap = r1.overlaps(r2)
        isFullOverlap -> isPartialOverlap
      }
      .foldLeft(0 -> 0) { case ((sum1, sum2), (is1, is2)) =>
        (sum1 + booleanToInt(is1)) -> (sum2 + booleanToInt(is2))
      }

    println(
      s"""Sum of assignment pairs with
         |  full overlaps:     $fullOverlaps
         |  partial overlaps:  $partialOverlaps""".stripMargin
    )
  }
}
