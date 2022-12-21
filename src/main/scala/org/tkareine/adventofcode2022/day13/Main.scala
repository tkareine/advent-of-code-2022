package org.tkareine.adventofcode2022.day13

import java.io.Reader
import java.nio.file.Path
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.{Try, Using, Failure as Err, Success as Ok}

enum Packet {
  case O(i: Int)
  case M(ps: List[Packet])
}

given packetOrdering: Ordering[Packet] with {
  import Packet.*

  override def compare(left: Packet, right: Packet): Int =
    (left, right) match {
      case (O(d1), O(d2)) =>
        d1 - d2

      case (_: O, _: M) =>
        compare(M(List(left)), right)

      case (_: M, _: O) =>
        compare(left, M(List(right)))

      case (M(ds1), M(ds2)) =>
        val order = ds1
          .zip(ds2)
          .view
          .map { case (d1, d2) => compare(d1, d2) }
          .find(_ != 0)
        order.getOrElse { ds1.size - ds2.size }
    }
}

object PacketParser extends RegexParsers {
  import Packet.*

  def integer: Parser[Int] =
    """\d+""".r ^^ { _.toInt }

  def dataO: Parser[O] =
    integer ^^ O.apply

  def dataOM: Parser[Packet] =
    dataO | dataM

  def dataM: Parser[M] =
    "[" ~> repsep(dataOM, ",") <~ "]" ^^ M.apply

  def dataMs: Parser[List[M]] =
    rep(dataM)

  def parse(in: String): Try[List[M]] =
    parse(dataMs, in).toTry

  def parse(in: Reader): Try[List[M]] =
    parse(dataMs, in).toTry

  extension [T](pr: ParseResult[T]) {
    def toTry: Try[T] =
      pr match {
        case Success(result, _) => Ok(result)
        case Failure(msg, _)    => Err(RuntimeException(msg))
        case Error(msg, _)      => Err(RuntimeException(msg))
      }
  }
}

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day13.txt")

@main def main(): Unit = {
  val packets = Using(Source.fromFile(InputFile.toFile)) { file =>
    PacketParser.parse(file.bufferedReader()).get
  }.get

  val sumOfPacketPairsInOrder = packets
    .grouped(2)
    .map(ps => ps.head -> ps(1))
    .zipWithIndex
    .filter { case ((leftP, rightP), _) => packetOrdering.compare(leftP, rightP) < 0 }
    .map { case (_, idx) => idx + 1 }
    .sum

  val decoderKey = {
    val dividers = PacketParser.parse("[[2]] [[6]]").get
    val ordered = (dividers ++ packets).sorted.zipWithIndex
    val div1Idx = ordered.find { case (p, _) => p == dividers.head }.get._2 + 1
    val div2Idx = ordered.find { case (p, _) => p == dividers(1) }.get._2 + 1
    div1Idx * div2Idx
  }

  println(
    s"""Sum of packet pairs in order: $sumOfPacketPairsInOrder
       |Decoder key: $decoderKey""".stripMargin
  )
}
