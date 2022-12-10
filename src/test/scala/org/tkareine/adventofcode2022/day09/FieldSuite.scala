package org.tkareine.adventofcode2022.day09

import org.scalatest.funsuite.AnyFunSuite

class FieldSuite extends AnyFunSuite {
  import Direction.*

  test("Field.move D 2") {
    val actual = Field(RopePos(10)).move(RopeMotion(Down, 2)).toPrettyString(0, 3, 0, 0)
    val expected = "2\n1\nH\n."
    assert(actual === expected)
  }

  test("Field.move R 4") {
    val actual = Field(RopePos(10)).move(RopeMotion(Right, 4)).toPrettyString(5, 0, 0, 0)
    val expected = "4321H."
    assert(actual === expected)
  }

  test("Field.move L 7") {
    val ropePos = Vector(
      5 -> 0,
      4 -> 1,
      3 -> 2,
      2 -> 2,
      1 -> 3,
      0 -> 4
    )
    val actual = Field(ropePos).move(RopeMotion(Left, 7)).toPrettyString(5, 3, -5, 0)
    val expected =
      """...H123....
        |......4....
        |......5....
        |...........""".stripMargin
    assert(actual === expected)
  }
}
