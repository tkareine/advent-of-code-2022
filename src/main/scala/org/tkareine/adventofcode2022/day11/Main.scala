package org.tkareine.adventofcode2022.day11

import java.io.Reader
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.BufferedIterator
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.{Try, Using, Failure as Err, Success as Ok}

case class Monkey(
    items: List[Long],
    operation: Expr,
    throwTest: ThrowTest
)

enum Operand {
  case Const(n: Int)
  case Old

  def apply(n: Long): Long =
    this match {
      case Const(c) => c
      case Old      => n
    }
}

enum Expr {
  case Add(o1: Operand, o2: Operand)
  case Multiply(o1: Operand, o2: Operand)

  def apply(n: Long): Long =
    this match {
      case Add(o1, o2)      => o1(n) + o2(n)
      case Multiply(o1, o2) => o1(n) * o2(n)
    }
}

case class ToMonkey(index: Int)

case class ThrowTest(divisibleBy: Int, ifTrue: ToMonkey, ifFalse: ToMonkey) {
  def apply(n: Long): Int =
    if n % divisibleBy == 0 then ifTrue.index else ifFalse.index
}

object MonkeyParser extends RegexParsers {
  import Expr.*
  import Operand.*

  def integer: Parser[Int] =
    """\d+""".r ^^ { _.toInt }

  def items: Parser[List[Int]] =
    "Starting items:" ~> repsep(integer, ",")

  def operand: Parser[Operand] =
    (integer | "old") ^^ {
      case i: Int => Const(i)
      case _      => Old
    }

  def operation: Parser[Expr] =
    "Operation:" ~> "new" ~> "=" ~> operand ~ ("+" | "*") ~ operand ^^ {
      case o1 ~ "+" ~ o2 => Add(o1, o2)
      case o1 ~ "*" ~ o2 => Multiply(o1, o2)
      case _             => sys.error("should not happen")
    }

  def throwIfTrue: Parser[ToMonkey] =
    "If true:" ~> "throw to monkey" ~> integer ^^ { ToMonkey.apply }

  def throwIfFalse: Parser[ToMonkey] =
    "If false:" ~> "throw to monkey" ~> integer ^^ { ToMonkey.apply }

  def throwTest: Parser[ThrowTest] =
    "Test:" ~> "divisible by" ~> integer ~ throwIfTrue ~ throwIfFalse ^^ { case i ~ tit ~ tif =>
      ThrowTest(divisibleBy = i, ifTrue = tit, ifFalse = tif)
    }

  def monkey: Parser[(Int, Monkey)] =
    "Monkey" ~> integer ~ ":" ~ items ~ operation ~ throwTest ^^ { case id ~ _ ~ is ~ op ~ tt =>
      id -> Monkey(items = is.map(_.toLong), operation = op, throwTest = tt)
    }

  def monkeys: Parser[List[(Int, Monkey)]] =
    rep(monkey)

  def parse(in: String): Try[List[(Int, Monkey)]] =
    parse(monkeys, in).toTry

  def parse(in: Reader): Try[List[(Int, Monkey)]] =
    parse(monkeys, in).toTry

  extension [T](pr: ParseResult[T])
    def toTry: Try[T] =
      pr match {
        case Success(result, _) => Ok(result)
        case Failure(msg, _)    => Err(RuntimeException(msg))
        case Error(msg, _)      => Err(RuntimeException(msg))
      }
}

case class RoundResult(monkeys: Vector[Monkey], inspections: Vector[Long]) {
  def addInspections(insp: Vector[Long]): RoundResult =
    copy(inspections = inspections.zip(insp).map { case (i1, i2) => i1 + i2 })
}

object RoundResult {
  def apply(monkeys: Vector[Monkey]): RoundResult =
    RoundResult(monkeys = monkeys, inspections = Vector.fill(monkeys.size)(0L))
}

def playRound(monkeys: Vector[Monkey])(reliefWorry: Long => Long): RoundResult = {
  @tailrec def go(index: Int, monkeys: Vector[Monkey], inspections: Vector[Long]): RoundResult =
    if (index == monkeys.size) {
      RoundResult(monkeys, inspections)
    } else if (monkeys(index).items.isEmpty) {
      go(index + 1, monkeys, inspections)
    } else {
      val fromMonkey = monkeys(index)
      val oldWorry = fromMonkey.items.head
      val newWorry = reliefWorry(fromMonkey.operation(oldWorry))
      val toIndex = fromMonkey.throwTest(newWorry)
      val toMonkey = monkeys(toIndex)
      go(
        index,
        monkeys
          .updated(index, fromMonkey.copy(items = fromMonkey.items.tail))
          .updated(toIndex, toMonkey.copy(items = toMonkey.items.appended(newWorry))),
        inspections.updated(index, inspections(index) + 1)
      )
    }

  go(0, monkeys, Vector.fill(monkeys.size)(0L))
}

def playRounds(times: Int, monkeys: Vector[Monkey])(reliefWorry: Long => Long) =
  (0 until times).foldLeft(RoundResult(monkeys)) { (rr, _) =>
    playRound(rr.monkeys)(reliefWorry).addInspections(rr.inspections)
  }

def monkeyBusiness(inspections: Vector[Long]): Long =
  inspections.sorted(Ordering[Long].reverse).take(2).product

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day11.txt")

@main def main(): Unit = {
  val monkeys = Using(Source.fromFile(InputFile.toFile)) { file =>
    MonkeyParser.parse(file.bufferedReader()).get
  }.get.map { case (_, m) => m }.toVector

  val divisorProduct = monkeys.map(_.throwTest.divisibleBy).product

  val monkeyBusiness20R = monkeyBusiness(playRounds(20, monkeys)(_ / 3).inspections)

  val monkeyBusiness10_000R = monkeyBusiness(playRounds(10_000, monkeys)(_ % divisorProduct).inspections)

  println(
    s"""Monkey business:
       |      20 rounds: $monkeyBusiness20R
       |  10 000 rounds: $monkeyBusiness10_000R""".stripMargin
  )
}
