package InputParsing

import CommandExecution.RunRobotScript
import scala.io.Source

object ConsoleInteraction {
  def begin(): Unit = {
    println("Entering Console mode.")
    RunRobotScript.execute(CommandParsing.toRobotCommands(Source.stdin.getLines()))
  }
}