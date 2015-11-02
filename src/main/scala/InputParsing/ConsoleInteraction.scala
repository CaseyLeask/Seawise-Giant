package InputParsing

import scala.io.Source

object ConsoleInteraction {
  def begin(): Unit = {
    println("Entering Console mode.")
    CommandParsing.toRobotCommands(Source.stdin.getLines())
  }
}