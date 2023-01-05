package org.tkareine.adventofcode2022.day15

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class SensorReadingSuite extends AnyFunSuite with TableDrivenPropertyChecks {
  test("coverage") {
    assert(SensorReading(sensorAt = 0 -> 1, 4 -> 5).coverage === 8)
  }

  test("xCoverageAt") {
    forAll(
      Table(
        ("sensorAt", "closestBeaconAt", "xCoverageAt", "expectedXRange"),
        (0 -> 0, 4 -> 0, 4, 0 to 0),
        (0 -> 1, 4 -> 5, 4, -5 to 5),
        (0 -> 1, 4 -> 5, 8, -1 to 1),
        (0 -> 1, 4 -> 5, 9, 0 to 0),
        (0 -> 1, 4 -> 5, 10, RangeOps.empty),
        (0 -> 1, 4 -> 5, -7, 0 to 0),
        (0 -> 1, 4 -> 5, -8, RangeOps.empty),
        (1 -> 1, -2 -> -2, -2, -2 to 4)
      )
    ) { (s, b, y, r) =>
      assert(SensorReading(sensorAt = s, closestBeaconAt = b).xCoverageAt(y) === r)
    }
  }
}
