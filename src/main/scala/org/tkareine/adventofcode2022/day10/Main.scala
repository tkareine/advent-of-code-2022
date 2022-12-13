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

case class Program(
    instructions: List[Instruction],
    registerX: Int = 1,
    nextInstructionScheduledAtCycleNum: Int = -1
) {
  import Instruction.*

  def run(numCycles: Int): (Program, Int) = {
    var x = registerX

    def apply(instruction: Instruction): Unit =
      instruction match {
        case Addx(n) => x += n
        case Noop    =>
      }

    var is = instructions
    var cycleNum = 0

    var nextInstructionAtCycleNum = nextInstructionScheduledAtCycleNum

    while (is.nonEmpty && cycleNum < numCycles) {
      if (nextInstructionAtCycleNum == -1) {
        nextInstructionAtCycleNum = cycleNum + is.head.cyclesConsumed - 1
      }

      if (cycleNum == nextInstructionAtCycleNum) {
        apply(is.head)
        is = is.tail
        nextInstructionAtCycleNum = -1
      }

      cycleNum += 1
    }

    val cyclesLeft = numCycles - cycleNum

    val nextProgram = Program(
      instructions = is,
      registerX = x,
      nextInstructionScheduledAtCycleNum = if (nextInstructionAtCycleNum != -1) {
        nextInstructionAtCycleNum - numCycles
      } else -1
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

def drawScreen(program: Program): String = {
  val crtWidth = 40

  def drawPixel(registerX: Int, cycleNum: Int): Char = {
    val x = (cycleNum - 1) % crtWidth
    if (registerX - 1 to registerX + 1).contains(x) then '#' else '.'
  }

  def isRowEnd(cycleNum: Int): Boolean =
    (cycleNum - 1) % crtWidth == crtWidth - 1

  val sb = StringBuilder()

  var prog = program
  var cycleNum = 1

  while (prog.instructions.nonEmpty) {
    sb.addOne(drawPixel(prog.registerX, cycleNum))

    if (isRowEnd(cycleNum)) {
      sb.addOne('\n')
    }

    val (nextPrg, _) = prog.run(1)
    prog = nextPrg
    cycleNum += 1
  }

  sb.toString()
}

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day10.txt")

@main def main(): Unit = {
  val instructions = Using(Source.fromFile(InputFile.toFile)) { file =>
    file.getLines().map(line => Instruction.parse(line.trim())).toList
  }.get

  val signalStrengths = measureSignals(Program(instructions), (20 to 220 by 40).toList)
  val screen = drawScreen(Program(instructions))

  println(
    s"""Sum of signal strengths: ${signalStrengths.sum}
       |
       |$screen""".stripMargin
  )
}
