package org.tkareine.adventofcode2022.day08

import java.nio.file.Path
import scala.io.Source
import scala.util.Using

type TreeHeights = Vector[Vector[Int]]

def parseTreeHeights(iter: Iterator[String]): TreeHeights =
  iter.foldLeft(Vector.empty[Vector[Int]]) { (acc, line) =>
    acc :+ line.map(Character.getNumericValue).toVector
  }

enum Direction:
  case Left, Right, Up, Down

def treeLine(numRows: Int, numCols: Int)(direction: Direction, xy: (Int, Int)): Seq[(Int, Int)] = {
  import Direction.*

  val (x, y) = xy

  direction match {
    case Left =>
      (0 until y).map(x -> _).reverse

    case Right =>
      ((y + 1) until numCols).map(x -> _)

    case Up =>
      (0 until x).map(_ -> y).reverse

    case Down =>
      ((x + 1) until numRows).map(_ -> y)
  }
}

def isVisible(ths: TreeHeights): (height: Int, xy: (Int, Int)) => Boolean = {
  val tl = treeLine(numRows = ths.size, numCols = ths.headOption.getOrElse(Vector.empty).size)

  def check(height: Int, tl: Seq[(Int, Int)]): Boolean =
    tl.forall { case (x, y) => ths(x)(y) < height }

  (height: Int, xy: (Int, Int)) => {
    import Direction.*

    check(height, tl(Left, xy)) ||
    check(height, tl(Right, xy)) ||
    check(height, tl(Up, xy)) ||
    check(height, tl(Down, xy))
  }
}

def scenicScore(ths: TreeHeights): (height: Int, xy: (Int, Int)) => Int = {
  val tl = treeLine(numRows = ths.size, numCols = ths.headOption.getOrElse(Vector.empty).size)

  def viewingDistance(height: Int, tl: Seq[(Int, Int)]): Int =
    if (tl.isEmpty) {
      0
    } else {
      val vd = tl.takeWhile { case (x, y) => ths(x)(y) < height }.size
      if vd == tl.size then vd else vd + 1
    }

  (height: Int, xy: (Int, Int)) => {
    import Direction.*

    viewingDistance(height, tl(Left, xy)) *
      viewingDistance(height, tl(Right, xy)) *
      viewingDistance(height, tl(Up, xy)) *
      viewingDistance(height, tl(Down, xy))
  }
}

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day08.txt")

@main def main(): Unit = {
  val ths = Using(Source.fromFile(InputFile.toFile)) { file =>
    parseTreeHeights(file.getLines().map(_.trim))
  }.get

  val treesVisible = {
    val check = isVisible(ths)
    for
      (row, x) <- ths.zipWithIndex
      (height, y) <- row.zipWithIndex
      if check(height, x -> y)
    yield (x, y)
  }.size

  val maxScenicScore = {
    val score = scenicScore(ths)
    for
      (row, x) <- ths.zipWithIndex
      (height, y) <- row.zipWithIndex
    yield score(height, x -> y)
  }.max

  println(
    s"""Trees visible: $treesVisible
       |Max scenic score: $maxScenicScore""".stripMargin
  )
}
