package org.tkareine.adventofcode2022.day03

import java.nio.file.Path
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Using}

def itemPriority(item: Char): Int =
  item.toInt - (if item.isUpper then 38 else 96)

def itemPriorityFromRucksack(rucksack: String): Int = {
  require(rucksack.length % 2 == 0)
  val comp1 = rucksack.substring(0, rucksack.length / 2).toSet
  val comp2 = rucksack.substring(rucksack.length / 2, rucksack.length).toSet
  (comp1 & comp2).headOption.map(itemPriority).getOrElse(0)
}

def badgePriorityFromRucksacks(r1: String, r2: String, r3: String): Int =
  (r1.toSet & r2.toSet & r3.toSet).headOption.map(itemPriority).getOrElse(0)

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day03.txt")

@main def main(): Unit = {
  Using(Source.fromFile(InputFile.toFile)) { file =>
    val rucksacks = file.getLines().toList

    require(rucksacks.length % 3 == 0)

    val prioritySum = rucksacks.map(itemPriorityFromRucksack).sum
    val badgeSum = rucksacks.grouped(3).map(g => badgePriorityFromRucksacks(g(0), g(1), g(2))).sum

    println(
      s"""Priorities sum: $prioritySum
         |Badges sum:     $badgeSum""".stripMargin
    )
  }
}
