package InputParsing

import CommandExecution.RunRobotScript

import scala.io.Source
import scala.util.{Failure, Success, Try}

object ReadingFromFile {
  def Read(fromFile: String): Unit = {
    println("Reading file " + fromFile)
    Try(Source.fromFile(fromFile).getLines()) match {
      case Success(lines: Iterator[String]) =>
        RunRobotScript.execute(CommandParsing.toRobotCommands(lines))
      case Failure(e: Throwable) =>
        println("Encountered exception: " + e.getMessage)
    }

  }
}
