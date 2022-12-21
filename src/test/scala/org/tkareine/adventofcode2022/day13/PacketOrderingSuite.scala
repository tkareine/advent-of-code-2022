package org.tkareine.adventofcode2022.day13

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class PacketOrderingSuite extends AnyFunSuite with TableDrivenPropertyChecks {
  private def packets: TableFor2[String, Int] =
    Table(
      ("input", "expectedOrdering"),
      (
        """[1,1,3,1,1]
          |[1,1,5,1,1]""".stripMargin,
        -1
      ),
      (
        """[[1],[2,3,4]]
          |[[1],4]""".stripMargin,
        -1
      ),
      (
        """[9]
          |[[8,7,6]]""".stripMargin,
        1
      ),
      (
        """[[4,4],4,4]
          |[[4,4],4,4,4]""".stripMargin,
        -1
      ),
      (
        """[7,7,7,7]
          |[7,7,7]""".stripMargin,
        1
      ),
      (
        """[]
          |[3]""".stripMargin,
        -1
      ),
      (
        """[[[]]]
          |[[]]""".stripMargin,
        1
      ),
      (
        """[1,[2,[3,[4,[5,6,7]]]],8,9]
          |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin,
        1
      ),
      (
        """[[4,4],4,4]
          |[[4,4],4,4]""".stripMargin,
        0
      )
    )

  test("packetOrder") {
    forAll(packets) { (input, expectedOrdering) =>
      val ps = PacketParser.parse(input).get.take(2)
      assert(packetOrdering.compare(ps.head, ps(1)).sign == expectedOrdering)
    }
  }
}
