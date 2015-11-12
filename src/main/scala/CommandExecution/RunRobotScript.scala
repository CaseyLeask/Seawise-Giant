package CommandExecution

import Commands.RobotCommand
import State.{RobotResult, InvalidState, RobotState}

object RunRobotScript {
  def execute(commands: Iterator[RobotCommand]): RobotState = {
    commands.foldLeft[RobotState](InvalidState)((state, command) => {
      RobotState.nextResult(state, command) match {
        case RobotResult(s, Some(string)) => println(string); s
        case RobotResult(s, None) => s
      }
    })
  }
}
