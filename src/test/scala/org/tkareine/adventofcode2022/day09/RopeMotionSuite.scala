package org.tkareine.adventofcode2022.day09

import org.scalatest.funsuite.AnyFunSuite

class RopeMotionSuite extends AnyFunSuite {
  import Direction.*

  test("RopeMotion.move") {
    assert(RopeMotion(Down, 1).move(Vector(0 -> 1, 0 -> 0)) === List(Vector(0 -> 2, 0 -> 1)))

    assert(RopeMotion(Down, 2).move(Vector(0 -> 1, 0 -> 0)) === List(Vector(0 -> 2, 0 -> 1), Vector(0 -> 3, 0 -> 2)))

    assert(RopeMotion(Down, 1).move(Vector(0 -> 2, 0 -> 1, 0 -> 0)) === List(Vector(0 -> 3, 0 -> 2, 0 -> 1)))

    assert(
      RopeMotion(Down, 2).move(Vector(0 -> 2, 0 -> 1, 0 -> 0)) === List(
        Vector(0 -> 3, 0 -> 2, 0 -> 1),
        Vector(0 -> 4, 0 -> 3, 0 -> 2)
      )
    )

    assert(RopeMotion(Right, 1).move(Vector(0 -> 1, 0 -> 0)) === List(Vector(1 -> 1, 0 -> 0)))
  }
}
