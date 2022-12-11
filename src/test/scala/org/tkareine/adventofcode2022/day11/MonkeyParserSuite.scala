package org.tkareine.adventofcode2022.day11

import org.scalatest.funsuite.AnyFunSuite
import org.tkareine.adventofcode2022.day11.Expr.Multiply
import org.tkareine.adventofcode2022.day11.Operand.{Const, Old}

class MonkeyParserSuite extends AnyFunSuite {
  test("parse") {
    val actual = MonkeyParser
      .parse(
        """Monkey 2:
        |  Starting items: 79, 98
        |  Operation: new = old * 19
        |  Test: divisible by 23
        |    If true: throw to monkey 2
        |    If false: throw to monkey 3""".stripMargin
      )
      .get
    val expected = List(
      2 -> Monkey(
        items = List(79, 98),
        operation = Multiply(Old, Const(19)),
        throwTest = ThrowTest(divisibleBy = 23, ifTrue = ToMonkey(2), ifFalse = ToMonkey(3))
      )
    )
    assert(actual == expected)
  }
}
