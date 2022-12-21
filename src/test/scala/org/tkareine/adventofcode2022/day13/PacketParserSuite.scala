package org.tkareine.adventofcode2022.day13

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}
import org.tkareine.adventofcode2022.day13.Packet.{M, O}

class PacketParserSuite extends AnyFunSuite with TableDrivenPropertyChecks {
  import Packet.*

  private def inputs: TableFor2[String, List[Packet]] =
    Table(
      ("input", "expectedPackets"),
      (
        """[[1],[2,3,4]]
          |[[1],4]""".stripMargin,
        List(
          M(List(M(List(O(1))), M(List(O(2), O(3), O(4))))),
          M(List(M(List(O(1))), O(4)))
        )
      ),
      (
        """[[[]]]
          |[[]]""".stripMargin,
        List(
          M(List(M(List(M(List()))))),
          M(List(M(List())))
        )
      )
    )

  test("parse") {
    forAll(inputs) { (input, expectedPackets) =>
      assert(PacketParser.parse(input).get == expectedPackets)
    }
  }
}
