package org.tkareine.adventofcode2022.day05

import java.nio.file.Path
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

type Stacks = Map[Char, List[Char]]

def parseStacks(lines: List[String]): Stacks = {
  val stacksByIndex = lines.head.iterator.zipWithIndex
    .filter { case (c, _) => c.isLetterOrDigit }
    .map { case (code, idx) => idx -> (code -> List.empty[Char]) }
    .toMap

  lines.tail
    .foldLeft(stacksByIndex) { (sbi, line) =>
      sbi.foldLeft(sbi) { case (sbi2, (i, (code, stack))) =>
        val c = line.charAt(i)
        if (c.isLetterOrDigit) {
          sbi2 + (i -> (code -> (c :: stack)))
        } else {
          sbi2
        }
      }
    }
    .values
    .toMap
}

enum CrateMoveStrategy {
  case ByOne, AllOnce

  def move(numCrates: Int, from: List[Char], to: List[Char]): (List[Char], List[Char]) = {
    val (crates, newFrom) = from.splitAt(numCrates)

    val newTo = this match {
      case ByOne   => crates.reverse ++ to
      case AllOnce => crates ++ to
    }

    newFrom -> newTo
  }
}

case class CrateMoveOp(numCrates: Int, fromStack: Char, toStack: Char) {
  def execute(stacks: Stacks, strategy: CrateMoveStrategy): Stacks = {
    val (newFrom, newTo) = strategy.move(numCrates, stacks(fromStack), stacks(toStack))
    stacks + (fromStack -> newFrom) + (toStack -> newTo)
  }
}

object CrateMoveOp {
  def parse(line: String): CrateMoveOp = {
    val parts = line.split("""\s+""", 6)
    CrateMoveOp(numCrates = parts(1).toInt, fromStack = parts(3).head, toStack = parts(5).head)
  }
}

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day05.txt")

@main def main(): Unit = {
  def topCratesOf(s: Stacks): List[Char] =
    s.toList
      .map { (code, stack) => code -> stack.headOption.getOrElse('_') }
      .sortBy(_._1)
      .map(_._2)

  Using(Source.fromFile(InputFile.toFile)) { file =>
    val linesIter = file.getLines()

    val initStacks = parseStacks(linesIter.takeWhile(_.nonEmpty).toList.reverse)

    val crateOps = linesIter.takeWhile(_ != null).map(CrateMoveOp.parse).toList

    val stacksMovedByOne = crateOps.foldLeft(initStacks) { (ss, op) => op.execute(ss, CrateMoveStrategy.ByOne) }
    val stacksMovedAllOnce = crateOps.foldLeft(initStacks) { (ss, op) => op.execute(ss, CrateMoveStrategy.AllOnce) }

    println(
      s"""Top crates moved by:
         |  one at a time: ${topCratesOf(stacksMovedByOne).mkString}
         |  many at once:  ${topCratesOf(stacksMovedAllOnce).mkString}""".stripMargin
    )
  }
}
