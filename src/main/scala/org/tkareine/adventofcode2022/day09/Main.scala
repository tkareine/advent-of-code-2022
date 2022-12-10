package org.tkareine.adventofcode2022.day09

import java.nio.file.Path
import java.text.ParseException
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

type XY = (Int, Int)

enum Direction:
  case Left, Right, Up, Down

object Direction {
  def fromString(str: String): Direction =
    str match {
      case "L" => Left
      case "R" => Right
      case "U" => Up
      case "D" => Down
      case _   => throw ParseException(s"Unrecognized direction: $str", 0)
    }
}

type RopePos = Vector[XY]

def RopePos(length: Int): RopePos = {
  require(length > 0)
  Vector.fill(length)(0 -> 0)
}

case class RopeMotion(direction: Direction, steps: Int) {
  import Direction.*
  import RopeMotion.*

  def move(pos: RopePos): Seq[RopePos] = {
    val (_, poss) = headMoves(pos.head).foldLeft(pos -> List.empty[RopePos]) { case ((prevPos, res), headXY) =>
      val nextPos = (headXY :: tailMoves(prevPos.tail, headXY)).toVector
      nextPos -> (nextPos :: res)
    }

    poss.reverse
  }

  def headMoves(xy: XY): Seq[XY] = {
    val (x, y) = xy
    direction match {
      case Left  => ((x - 1) to (x - steps) by -1).map(_ -> y)
      case Right => ((x + 1) to (x + steps)).map(_ -> y)
      case Up    => ((y - 1) to (y - steps) by -1).map(x -> _)
      case Down  => ((y + 1) to (y + steps)).map(x -> _)
    }
  }
}

object RopeMotion {
  def parse(line: String): RopeMotion = {
    line.split("""\s+""", 3).toList match {
      case d :: s :: _ =>
        RopeMotion(direction = Direction.fromString(d), steps = s.toInt)

      case _ =>
        throw ParseException(s"Unrecognized move: $line", 0)
    }
  }

  def tailMove(tailXY: XY, headXY: XY): XY = {
    val (tx, ty) = tailXY
    val (hx, hy) = headXY

    val dHx = hx - tx
    val dHy = hy - ty

    if (ty == hy && dHx.abs > 1) {
      val dx = if dHx < 0 then -1 else 1
      (tx + dx) -> ty
    } else if (tx == hx && dHy.abs > 1) {
      val dy = if dHy < 0 then -1 else 1
      tx -> (ty + dy)
    } else if (math.max(dHx.abs, dHy.abs) > 1 && math.min(dHx.abs, dHy.abs) > 0) {
      val dx = if dHx < 0 then -1 else 1
      val dy = if dHy < 0 then -1 else 1
      (tx + dx) -> (ty + dy)
    } else {
      tailXY
    }
  }

  def tailMoves(tail: Seq[XY], headXY: XY): List[XY] = {
    @tailrec
    def go(tail: List[XY], headXY: XY, res: List[XY]): List[XY] =
      tail match {
        case tailFromXY :: rest =>
          val tailToXY = tailMove(tailFromXY, headXY)
          go(rest, tailToXY, tailToXY :: res)
        case _ =>
          res
      }

    go(tail.toList, headXY, List.empty).reverse
  }
}

case class Field private (ropePos: RopePos, tailVisits: Set[XY]) {
  def move(motions: Seq[RopeMotion]): Field =
    motions.foldLeft(this) { (field, motion) =>
      field.move(motion)
    }

  def move(ropeMotion: RopeMotion): Field =
    ropeMotion.move(ropePos).foldLeft(this) { (field, rp) =>
      Field(ropePos = rp, tailVisits = field.tailVisits + rp.last)
    }

  def toPrettyString(maxX: Int, maxY: Int): String =
    toPrettyString(maxX, maxY, -maxX, -maxY)

  def toPrettyString(maxX: Int, maxY: Int, minX: Int, minY: Int): String = {
    val ropePosInField = ropePos.zipWithIndex.foldLeft(Map.empty[XY, Char]) { case (map, (xy, idx)) =>
      val c = if idx == 0 then 'H' else Character.forDigit(idx, 10)
      if (map.contains(xy)) {
        map
      } else {
        map + (xy -> c)
      }
    }

    val sb = StringBuilder()

    for
      y <- minY to maxY
      x <- minX to maxX
    do
      if (y > minY && x == minX) {
        sb.addOne('\n')
      }
      sb.addOne(ropePosInField.getOrElse(x -> y, '.'))

    sb.toString()
  }
}

object Field {
  def apply(ropePos: RopePos): Field =
    new Field(ropePos = ropePos, tailVisits = Set(ropePos.last))
}

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day09.txt")

@main def main(): Unit = {
  val motions = Using(Source.fromFile(InputFile.toFile)) { file =>
    file.getLines().map(line => RopeMotion.parse(line.trim())).toList
  }.get

  val ropeLen2TailVisits = Field(ropePos = RopePos(length = 2)).move(motions).tailVisits.size

  val ropeLen10TailVisits = Field(ropePos = RopePos(length = 10)).move(motions).tailVisits.size

  println(
    s"""Rope length 2 tail visits:  $ropeLen2TailVisits
       |Rope length 10 tail visits: $ropeLen10TailVisits""".stripMargin
  )
}
