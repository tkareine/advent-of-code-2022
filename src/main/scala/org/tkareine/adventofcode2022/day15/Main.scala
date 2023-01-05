package org.tkareine.adventofcode2022.day15

import java.io.Reader
import java.nio.file.Path
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.{Try, Using, Failure as Err, Success as Ok}

object RangeOps {
  def empty: Range =
    0 until 0
}

type XY = (Int, Int)

case class SensorReading(sensorAt: XY, closestBeaconAt: XY) {
  def coverage: Int =
    manhattanDistance(sensorAt, closestBeaconAt)

  def xCoverageAt(y: Int): Range = {
    val cov = coverage
    val (xs, ys) = sensorAt
    val dy = ys - y
    val dc = cov - math.abs(dy)
    if dc >= 0 then (xs - dc) to (xs + dc) else RangeOps.empty
  }
}

def manhattanDistance(p: XY, q: XY): Int = {
  val (xp, yp) = p
  val (xq, yq) = q
  math.abs(xp - xq) + math.abs(yp - yq)
}

extension (r1: Range) {
  def fastIntersect(r2: Range): Range = {
    require(r1.step == r2.step, "Ranges must have the same step")
    val start = math.max(r1.start, r2.start)
    val end = math.min(r1.end, r2.end)
    if r1.isInclusive then Range.inclusive(start, end, r1.step) else Range(start, end, r1.step)
  }
}

def findMissingBeacon(xMax: Int, yMax: Int, srs: Seq[SensorReading]): Option[XY] = {
  val xRange = 0 to xMax
  val onesMask = BitSet.fromSpecific(xRange)
  val zeroesMask = new mutable.BitSet(xMax + 1).toImmutable
  val sensorsCoverage = new mutable.BitSet(xMax + 1)

  println(s"Finding missing beacon…")

  val yIter = (0 to yMax).reverseIterator

  while (yIter.hasNext) {
    val y = yIter.next()

    if (y % 1000 == 0) {
      println(s"  y=$y…")
    }

    sensorsCoverage &= zeroesMask

    for (sr <- srs) {
      sensorsCoverage ++= sr.xCoverageAt(y).fastIntersect(xRange)
    }

    if (sensorsCoverage.size <= xMax) {
      val x = (sensorsCoverage ^ onesMask).min
      return Some(x -> y)
    }
  }

  None
}

object SensorReadingParser extends RegexParsers {
  def integer: Parser[Int] =
    """-?\d+""".r ^^ { _.toInt }

  def point: Parser[XY] =
    "x=" ~> integer ~ "," ~ "y=" ~ integer ^^ { case x ~ _ ~ _ ~ y => x -> y }

  def sensorReading: Parser[SensorReading] =
    "Sensor at" ~> point ~ ":" ~ "closest beacon is at" ~ point ^^ { case s ~ _ ~ _ ~ b => SensorReading(s, b) }

  def parse(in: Reader): Try[List[SensorReading]] =
    parse(rep(sensorReading), in).toTry

  extension [T](pr: ParseResult[T]) {
    def toTry: Try[T] =
      pr match {
        case Success(result, _) => Ok(result)
        case Failure(msg, _)    => Err(RuntimeException(msg))
        case Error(msg, _)      => Err(RuntimeException(msg))
      }
  }
}

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day15.txt")

@main def main(): Unit = {
  val sensorReadings = Using(Source.fromFile(InputFile.toFile)) { file =>
    SensorReadingParser.parse(file.bufferedReader()).get
  }.get

  val y = 2_000_000

  val numPositionsCannotContainBeacon = sensorReadings
    .foldLeft(Set.empty[Int]) { (acc, sr) =>
      val (xb, yb) = sr.closestBeaconAt
      val acc2 = acc ++ sr.xCoverageAt(y)
      if yb == y then acc2 - xb else acc2
    }
    .size

  println(s"Number of positions cannot contain beacon at y=$y: $numPositionsCannotContainBeacon")

  val xyMax = 4_000_000

  val missingBeaconPos = findMissingBeacon(xyMax, xyMax, sensorReadings).get

  val frequency = {
    val (x, y) = missingBeaconPos
    x.toLong * xyMax + y
  }

  println(s"Missing beacon: xy=$missingBeaconPos, frequency=$frequency")
}
