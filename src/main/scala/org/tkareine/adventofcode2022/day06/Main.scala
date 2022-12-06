package org.tkareine.adventofcode2022.day06

import java.nio.file.Path
import scala.io.Source
import scala.util.Using

val StartOfPacketSize = 4
val StartOfMessageSize = 14

def indexOfMarker(markerSize: Int, cs: Seq[Char]): Int =
  cs.sliding(markerSize).takeWhile(grp => grp.distinct.size != markerSize).size

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day06.txt")

@main def main(): Unit = {
  Using(Source.fromFile(InputFile.toFile)) { file =>
    val chars = file.toList
    val startOfPacketIdx = indexOfMarker(StartOfPacketSize, chars) + StartOfPacketSize
    val startOfMessageIdx = indexOfMarker(StartOfMessageSize, chars) + StartOfMessageSize
    println(s"startOfPacketIdx=$startOfPacketIdx, startOfMessageIdx=$startOfMessageIdx")
  }
}
