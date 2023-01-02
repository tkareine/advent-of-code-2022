package org.tkareine.adventofcode2022.day14

import java.io.Reader
import java.nio.file.Path
import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.{Try, Using, Failure as Err, Success as Ok}

type XY = (Int, Int)

object PathParser extends RegexParsers {
  def integer: Parser[Int] =
    """\d+""".r ^^ { _.toInt }

  def point: Parser[XY] =
    integer ~ "," ~ integer ^^ { case x ~ _ ~ y => x -> y }

  def path: Parser[List[XY]] =
    rep1sep(point, "->")

  def paths: Parser[List[List[XY]]] =
    rep(path)

  def parse(in: Reader): Try[List[List[XY]]] =
    parse(paths, in).toTry

  extension [T](pr: ParseResult[T]) {
    def toTry: Try[T] =
      pr match {
        case Success(result, _) => Ok(result)
        case Failure(msg, _)    => Err(RuntimeException(msg))
        case Error(msg, _)      => Err(RuntimeException(msg))
      }
  }
}

trait Cave {
  def solids: Set[XY]

  protected def requireNonSolidPos(pos: XY): Unit =
    require(!solids.contains(pos), s"Cannot pour sand at position containing a solid: $pos")

  /** @return Pair of (did poured sand came to rest?, new cave possibly filled with poured sand) */
  def pourSandUnit(pos: XY): (Boolean, Cave)

  def fillWithSand(pouringPos: XY): (Int, Cave) = {
    @tailrec def go(sandUnitsFilled: Int, cave: Cave): (Int, Cave) = {
      if (cave.solids.contains(pouringPos)) {
        sandUnitsFilled -> cave
      } else {
        val (sandCameToRest, newCave) = cave.pourSandUnit(pouringPos)
        if (sandCameToRest) {
          go(sandUnitsFilled + 1, newCave)
        } else {
          sandUnitsFilled -> newCave
        }
      }
    }

    go(0, this)
  }
}

case class BottomlessCave(solids: Set[XY], maxY: Int) extends Cave {
  def pourSandUnit(pos: XY): (Boolean, Cave) = {
    requireNonSolidPos(pos)

    @tailrec def go(currPos: XY): (Boolean, Cave) = {
      val (_, y) = currPos
      if (y > maxY) {
        false -> this
      } else {
        fallingSandPositions(currPos).find(xy => !solids.contains(xy)) match {
          case Some(nextPos) => go(nextPos)
          case None          => true -> copy(solids = solids + currPos)
        }
      }
    }

    go(pos)
  }
}

object BottomlessCave {
  def apply(solids: Set[XY]): BottomlessCave =
    new BottomlessCave(
      solids = solids,
      maxY = solids.maxByOption { case (_, y) => y }.map(_._2).getOrElse(0)
    )
}

case class FlooredCave(solids: Set[XY], floorY: Int) extends Cave {
  def pourSandUnit(pos: XY): (Boolean, Cave) = {
    requireNonSolidPos(pos)

    @tailrec def go(currPos: XY): (Boolean, Cave) = {
      fallingSandPositions(currPos).find { case xy @ (_, y) => (y < floorY) && !solids.contains(xy) } match {
        case Some(nextPos) => go(nextPos)
        case None          => true -> copy(solids = solids + currPos)
      }
    }

    go(pos)
  }
}

object FlooredCave {
  def apply(solids: Set[XY]): FlooredCave =
    new FlooredCave(
      solids = solids,
      floorY = solids.maxByOption { case (_, y) => y }.map(_._2).getOrElse(0) + 2
    )
}

def fillSolids(paths: List[List[XY]]): Set[XY] =
  paths.foldLeft(Set.empty[XY]) { (acc, path) =>
    acc ++ path.zip(path.tail).foldLeft(Set.empty[XY]) { case (acc1, (start, end)) =>
      acc1 ++ positionsBetween(start, end)
    }
  }

def positionsBetween(p1: XY, p2: XY): Seq[XY] = {
  val (x1, y1) = p1
  val (x2, y2) = p2
  for {
    x <- math.min(x1, x2) to math.max(x1, x2)
    y <- math.min(y1, y2) to math.max(y1, y2)
  } yield x -> y
}

def fallingSandPositions(pos: XY): List[XY] = {
  val (x, y) = pos
  x -> (y + 1) :: (x - 1) -> (y + 1) :: (x + 1) -> (y + 1) :: Nil
}

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day14.txt")

val SandPouringPos = 500 -> 0

@main def main(): Unit = {
  val rockPaths = Using(Source.fromFile(InputFile.toFile)) { file =>
    PathParser.parse(file.bufferedReader()).get
  }.get

  val solids = fillSolids(rockPaths)

  println("Number of sand units filled when")
  println(s"  bottomless cave: ${BottomlessCave(solids = solids).fillWithSand(SandPouringPos)._1}")
  println(s"  floored cave:    ${FlooredCave(solids = solids).fillWithSand(SandPouringPos)._1}")
}
