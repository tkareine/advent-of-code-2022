package org.tkareine.adventofcode2022.day07

import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.{BufferedIterator, mutable}
import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

extension [A](iter: BufferedIterator[A])
  def takeWhileToList(p: A => Boolean): List[A] = {
    val res = mutable.ListBuffer.empty[A]
    while (iter.hasNext && p(iter.head)) {
      res += iter.next()
    }
    res.toList
  }

enum FileNode {
  case Dir(parent: Option[Dir], entries: mutable.Map[String, FileNode] = mutable.Map.empty)

  case File(size: Int)

  def nodeSize: Int =
    this match {
      case FileNode.File(size) =>
        size

      case FileNode.Dir(_, entries) =>
        entries.values.map(_.nodeSize).sum
    }

  def toPrettyString(depth: Int = 0): String = {
    def pp(node: FileNode, name: String, depth: Int): String = {
      val prefix = (" " * (2 * depth)) + name
      val suffix = node match {
        case Dir(_, entries) =>
          val es = entries.toList
            .sortBy(_._1)
            .map { case (name, node) => pp(node, name, depth + 1) }
            .mkString("\n")
          if es.isEmpty then "" else "\n" + es

        case File(size) =>
          " " + size.toString
      }
      prefix + suffix
    }

    val currentName = this match {
      case Dir(None, _) =>
        "/"

      case Dir(Some(_), _) =>
        "../"

      case File(_) =>
        "(unnamed file)"
    }

    pp(this, currentName, depth)
  }
}

object FileNode {
  def mkRoot(): FileNode.Dir =
    Dir(parent = None)

  @tailrec
  def rootOf(dir: FileNode.Dir): FileNode.Dir =
    dir.parent match {
      case Some(d) =>
        rootOf(d)

      case None =>
        dir
    }

  def flatten(dir: FileNode.Dir): List[FileNode.Dir] =
    dir.entries.values.toList.flatMap {
      case d: FileNode.Dir =>
        d :: flatten(d)
      case _: FileNode.File =>
        List.empty
    }
}

enum Command {
  case Ls
  case CdParent
  case CdRoot
  case CdDir(dirName: String)
}

object Command {
  def parse(line: String): Command =
    line.split("""\s+""").toList match {
      case "ls" :: _ =>
        Ls
      case "cd" :: ".." :: _ =>
        CdParent
      case "cd" :: "/" :: _ =>
        CdRoot
      case "cd" :: n :: _ =>
        CdDir(n)
      case _ =>
        throw UnsupportedOperationException(s"Unknown command: $line")
    }
}

def isCommand(line: String): Boolean =
  line.charAt(0) == '$'

val Digits: Regex = """(\d+)""".r

def parseLs(lines: Seq[String], cwd: FileNode.Dir): Map[String, FileNode] =
  lines.map { line =>
    line.split("""\s+""").toList match {
      case "dir" :: name :: _ =>
        name -> FileNode.Dir(parent = Some(cwd))

      case Digits(ds) :: name :: _ =>
        name -> FileNode.File(size = ds.toInt)

      case _ =>
        sys.error(s"Unexpected output: $line")
    }
  }.toMap

def parseFileTree(lines: BufferedIterator[String]): FileNode.Dir = {
  import Command.*

  var cwd = FileNode.mkRoot()

  while (lines.hasNext) {
    val line = lines.next()

    require(isCommand(line), s"Expected command: $line")

    cwd = Command.parse(line.tail.trim()) match {
      case Ls =>
        cwd.entries ++= parseLs(lines.takeWhileToList(!isCommand(_)), cwd)
        cwd

      case CdParent =>
        cwd.parent.getOrElse(cwd)

      case CdRoot =>
        FileNode.rootOf(cwd)

      case CdDir(name) =>
        cwd.entries.get(name) match {
          case Some(dir: FileNode.Dir) =>
            dir

          case _ =>
            sys.error(s"No such dir: $name")
        }
    }
  }

  FileNode.rootOf(cwd)
}

val InputFile = Path.of(sys.props("user.home"), "Scratches/advent-of-code-2022/input/day07.txt")

@main def main(): Unit = {
  import Command.*

  val rootDir = Using(Source.fromFile(InputFile.toFile)) { file =>
    parseFileTree(file.getLines().drop(1).map(_.trim).buffered)
  }.get

  val allDirSizes = FileNode.flatten(rootDir).map(_.nodeSize)

  val sumDirsMaxSize = allDirSizes.filter(_ < 100000).sum
  val diskSize = 70000000
  val diskSpaceFree = diskSize - rootDir.nodeSize
  val targetDiskSpaceFree = 30000000
  val deleteDirSize = allDirSizes.filter(diskSpaceFree + _ > targetDiskSpaceFree).min

  println(
    s"""part 1: $sumDirsMaxSize
       |part 2: $deleteDirSize""".stripMargin
  )
}
