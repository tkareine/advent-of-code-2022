package org.tkareine.adventofcode2022.day02

import scala.io.StdIn.readLine

enum Shape {
  case Rock, Paper, Scissors

  def score: Int =
    ordinal + 1
}

object Shape {
  def decode(c: Char): Shape = c match {
    case 'A' | 'X' => Rock
    case 'B' | 'Y' => Paper
    case 'C' | 'Z' => Scissors
    case _         => sys.error(s"Cannot decode: '$c'")
  }

  def requiredForOutcome(opponent: Shape, outcome: RoundOutcome): Shape = {
    import RoundOutcome._

    (outcome, opponent) match {
      case (Draw, _)        => opponent
      case (Win, Rock)      => Paper
      case (Win, Paper)     => Scissors
      case (Win, Scissors)  => Rock
      case (Lose, Rock)     => Scissors
      case (Lose, Paper)    => Rock
      case (Lose, Scissors) => Paper
    }
  }
}

enum RoundOutcome {
  case Lose, Draw, Win

  def score: Int =
    ordinal * 3
}

object RoundOutcome {
  import Shape._

  def decode(c: Char): RoundOutcome = c match {
    case 'X' => Lose
    case 'Y' => Draw
    case 'Z' => Win
    case _   => sys.error(s"Cannot decode: '$c'")
  }

  def roundOutcome(opponent: Shape, you: Shape): RoundOutcome = (opponent, you) match {
    case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => Win
    case (Paper, Rock) | (Scissors, Paper) | (Rock, Scissors) => Lose
    case _                                                    => Draw
  }

  def roundScore(opponent: Shape, you: Shape): Int =
    you.score + roundOutcome(opponent, you).score
}

// sbt 'runMain org.tkareine.adventofcode2022.day02.main' < input/day02.txt
@main def main(): Unit = {
  var totalScoreWhenCol2IsShape = 0L
  var totalScoreWhenCol2IsOutcome = 0L

  for (line <- Iterator.continually(readLine()).takeWhile(_ != null).map(_.strip())) {
    val (roundScoreWhenShape, roundScoreWhenOutcome) = (line.lift(0), line.lift(2)) match {
      case (Some(opponentC), Some(shapeOrOutcomeC)) =>
        val opponent = Shape.decode(opponentC)
        val youWhenShape = Shape.decode(shapeOrOutcomeC)
        val youWhenOutcome = Shape.requiredForOutcome(opponent, RoundOutcome.decode(shapeOrOutcomeC))
        RoundOutcome.roundScore(opponent, youWhenShape) -> RoundOutcome.roundScore(opponent, youWhenOutcome)

      case _ =>
        sys.error(s"Invalid line to decode: $line")
    }

    totalScoreWhenCol2IsShape += roundScoreWhenShape
    totalScoreWhenCol2IsOutcome += roundScoreWhenOutcome
  }

  println(
    s"""Total scores
       |  when col 2 is shape:    $totalScoreWhenCol2IsShape
       |  when col 2 is outcome:  $totalScoreWhenCol2IsOutcome""".stripMargin
  )
}
