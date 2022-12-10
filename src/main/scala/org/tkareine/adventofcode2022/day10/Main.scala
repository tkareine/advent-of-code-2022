package org.tkareine.adventofcode2022.day10

import java.nio.file.Path
import java.text.ParseException
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

enum Instruction(val cyclesConsumed: Int):
  case Noop extends Instruction(1)
  case Addx(n: Int) extends Instruction(2)

object Instruction {
  def parse(str: String): Instruction =
    str.split("""\s+""", 3).toList match {
      case "noop" :: _      => Noop
      case "addx" :: n :: _ => Addx(n.toInt)
      case _                => throw ParseException(s"Unknown instruction: $str", 0)
    }
}

case class Program(instructions: List[Instruction], registerX: Int = 1, headInstructionCyclesConsumed: Int = 0) {
  import Instruction.*

  require(headInstructionCyclesConsumed == 0 || (headInstructionCyclesConsumed > 0 && instructions.nonEmpty))

  def run(numCycles: Int): (Program, Int) = {
    var x = registerX

    def effect(instruction: Instruction): Unit =
      instruction match {
        case Addx(n) => x += n
        case Noop    =>
      }

    var is = instructions
    var currentCycle = 0

    var nextInstructionEffectAtCycle =
      if (headInstructionCyclesConsumed > 0) {
        is.head.cyclesConsumed - headInstructionCyclesConsumed
      } else -1

    while (is.nonEmpty && currentCycle < numCycles) {
      if (nextInstructionEffectAtCycle == -1) {
        nextInstructionEffectAtCycle = currentCycle + is.head.cyclesConsumed - 1
      }

      if (currentCycle == nextInstructionEffectAtCycle) {
        effect(is.head)
        is = is.tail
        nextInstructionEffectAtCycle = -1
      }

      currentCycle += 1
    }

    val cyclesLeft = numCycles - currentCycle - 1

    val nextProgram = Program(
      instructions = is,
      registerX = x,
      headInstructionCyclesConsumed = if (nextInstructionEffectAtCycle != -1) {
        nextInstructionEffectAtCycle - currentCycle
      } else 0
    )

    nextProgram -> cyclesLeft
  }
}

def signalStrength(cycleNum: Int, registerX: Int): Int =
  cycleNum * registerX

def measureSignals(program: Program, atCycleNums: Seq[Int]): Seq[Int] = {
  @tailrec
  def go(prog: Program, cyclesConsumed: Int, atCycleNums: List[Int], sigStrs: Vector[Int]): Vector[Int] =
    atCycleNums match {
      case cycleNum :: restCycleNums =>
        val cyclesToRun = cycleNum - cyclesConsumed
        val (nextProg, cyclesLeft) = prog.run(cyclesToRun)

        if (cyclesLeft > 0) {
          throw IllegalStateException("Program finished before measurement")
        }

        go(
          prog = nextProg,
          cyclesConsumed = cycleNum,
          atCycleNums = restCycleNums,
          sigStrs = sigStrs :+ signalStrength(cycleNum, nextProg.registerX)
        )

      case _ =>
        sigStrs
    }

  go(program, 0, atCycleNums.toList, Vector.empty)
}

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day10.txt")

@main def main(): Unit = {
  val instructions = Using(Source.fromFile(InputFile.toFile)) { file =>
    file.getLines().map(line => Instruction.parse(line.trim())).toList
  }.get

  val cyclesToMeasureSignal = (20 to 220 by 40).toList

  val signalStrengths = measureSignals(Program(instructions), cyclesToMeasureSignal)

  println(
    s"""Sum of signal strengths: ${signalStrengths.sum}""".stripMargin
  )
}
