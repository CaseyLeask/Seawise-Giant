package CommandExecution

import InputParsing._

sealed trait RobotState
case object InvalidState extends RobotState
case class ValidState(x: Int, y: Int, direction: Direction) extends RobotState

object RunRobotScript {
  val smallDimension = Tabletop.dimensions.head
  val largeDimension = Tabletop.dimensions.last

  def nextState(state: RobotState, command: RobotCommand): RobotState = (state, command) match {
    case (_, Place(x, y, direction)) => ValidState(x, y, direction)

    case (InvalidState, _) => InvalidState

    case (ValidState(x, y, direction), Report) => println(s"$x,$y,${direction.toString.toUpperCase}"); state

    case (ValidState(_, `largeDimension`, North), Move) => state
    case (ValidState(`largeDimension`, _, East ), Move) => state
    case (ValidState(_, `smallDimension`, South), Move) => state
    case (ValidState(`smallDimension`, _, West ), Move) => state

    case (ValidState(x, y, North), Move) => ValidState(x, y + 1, North)
    case (ValidState(x, y, East ), Move) => ValidState(x + 1, y, East )
    case (ValidState(x, y, South), Move) => ValidState(x, y - 1, South)
    case (ValidState(x, y, West ), Move) => ValidState(x - 1, y, West )

    case (ValidState(x, y, North), Left) => ValidState(x, y, West )
    case (ValidState(x, y, East ), Left) => ValidState(x, y, North)
    case (ValidState(x, y, South), Left) => ValidState(x, y, East )
    case (ValidState(x, y, West ), Left) => ValidState(x, y, South)

    case (ValidState(x, y, North), Right) => ValidState(x, y, East )
    case (ValidState(x, y, East ), Right) => ValidState(x, y, South)
    case (ValidState(x, y, South), Right) => ValidState(x, y, West )
    case (ValidState(x, y, West ), Right) => ValidState(x, y, North)
  }

  def execute(commands: Iterator[RobotCommand]): RobotState = {
    commands.foldLeft[RobotState](InvalidState)(nextState)
  }
}
