package org.tkareine.adventofcode2022.day01

import scala.collection.SortedSet
import scala.io.StdIn.readLine

// sbt 'runMain org.tkareine.adventofcode2022.day01.main' < input/calories-by-elves.txt
@main def main(): Unit = {
  var maxSums = scala.collection.mutable.SortedSet()(Ordering.Long.reverse)
  var currentSum = 0L

  for (line <- Iterator.continually(readLine()).takeWhile(_ != null).map(_.strip())) {
    if (line.isEmpty && currentSum > 0) {
      maxSums += currentSum
      maxSums = maxSums.take(3)
      currentSum = 0
    } else {
      currentSum += line.toLong
    }
  }

  println(
    s"""Top 3 total calories:   ${maxSums.mkString(", ")}
       |Sum of top 3 calories:  ${maxSums.sum}""".stripMargin
  )
}
