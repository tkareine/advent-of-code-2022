package org.tkareine.adventofcode2022.day12

import java.nio.file.Path
import java.util.Comparator
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

type XY = (Int, Int)

opaque type ElevationMap = Vector[Vector[Int]]

extension (v: ElevationMap)
  def apply(xy: XY): Int =
    val (x, y) = xy
    v(y)(x)

  def neighbours(xy: XY): List[XY] =
    val (x, y) = xy
    val baseElev = v(y)(x)
    val rows = v.indices
    val cols = v.head.indices
    for
      (dx, dy) <- List(0 -> -1, 1 -> 0, 0 -> 1, -1 -> 0)
      x2 = x + dx
      y2 = y + dy
      if rows.contains(y2) && cols.contains(x2) && v(y2)(x2) <= baseElev + 1
    yield x2 -> y2

  def positionsAtElevation(elevation: Int): List[XY] =
    var res = List.empty[XY]
    for
      (col, y) <- v.zipWithIndex
      (elev, x) <- col.zipWithIndex
      if elev == elevation
    do res = (x -> y) :: res
    res

object ElevationMap:
  def apply(v: Vector[Vector[Int]]): ElevationMap =
    v

case class Elevations(start: XY, end: XY, map: ElevationMap)

object Elevations:
  def parse(iter: Iterator[String]): Elevations =
    val baseElevation = 'a'.toInt

    def elevationOf(c: Char): Int =
      c.toInt - baseElevation

    var start: XY = null
    var end: XY = null

    val map = iter.zipWithIndex.foldLeft(Vector.empty[Vector[Int]]) { case (acc, (line, y)) =>
      val elevs = line.iterator.zipWithIndex.map { case (c, x) =>
        val c1 = c match
          case 'S' =>
            start = x -> y
            'a'

          case 'E' =>
            end = x -> y
            'z'

          case c if Character.isLowerCase(c) =>
            c

          case c =>
            sys.error(s"Unrecognized char '$c' at line: $line")

        elevationOf(c1)
      }

      acc :+ elevs.toVector
    }

    require(map.nonEmpty && map.head.nonEmpty, "Empty elevation map")
    require(start != null, "Start position not found from elevation map")
    require(end != null, "End position not found from elevation map")

    Elevations(start = start, end = end, map = ElevationMap(map))

/** [[https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm]] */
def shortestPath(start: XY, end: XY, elevationMap: ElevationMap): Option[List[XY]] =
  case class Pos(xy: XY, distance: Int)

  val distancesFromSource = mutable.Map(start -> 0)

  val previousXy = mutable.Map.empty[XY, XY]

  val posComparator = new Comparator[Pos]:
    def compare(a: Pos, b: Pos): Int =
      a.distance.compare(b.distance)

  val priorityQueue = java.util.PriorityQueue[Pos](posComparator)

  def setShorterDistance(from: XY, to: XY, distance: Int): Unit =
    distancesFromSource += (to -> distance)
    previousXy += (to -> from)
    priorityQueue.removeIf(p => p.xy == to)
    priorityQueue.add(Pos(to, distance))

  def search(): Unit =
    priorityQueue.add(Pos(start, 0))

    while (!priorityQueue.isEmpty)
      val Pos(from, _) = priorityQueue.remove()

      if (from == end)
        return // found
      else
        for to <- elevationMap.neighbours(from)
        do
          val newDist = distancesFromSource(from) + 1
          distancesFromSource.get(to) match
            case Some(d) if newDist < d =>
              setShorterDistance(from, to, newDist)
            case None =>
              setShorterDistance(from, to, newDist)
            case _ =>

  @tailrec def mkPath(to: XY, res: List[XY]): List[XY] =
    previousXy.get(to) match
      case Some(prev) =>
        mkPath(prev, to :: res)
      case None =>
        res

  search()

  if (previousXy.contains(end))
    Some(mkPath(end, Nil))
  else
    None

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day12.txt")

@main def main(): Unit =
  val elevations = Using(Source.fromFile(InputFile.toFile)) { file =>
    Elevations.parse(file.getLines())
  }.get

  val pathFromS = shortestPath(elevations.start, elevations.end, elevations.map).get

  val pathFromClosestPos = elevations.map
    .positionsAtElevation(0)
    .map(pos => shortestPath(pos, elevations.end, elevations.map))
    .filter(_.isDefined)
    .map(_.get)
    .minBy(_.size)

  println(
    s"""Shortest path length
       |  from S: ${pathFromS.size}
       |  from closest position: ${pathFromClosestPos.size}""".stripMargin
  )
