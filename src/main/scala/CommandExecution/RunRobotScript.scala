package CommandExecution

import InputParsing._

case class PlacedObject(x: Int, y: Int)

sealed trait RobotState
case object InvalidState extends RobotState
case class ValidState(x: Int, y: Int, direction: Direction, placedObjects: Set[PlacedObject]) extends RobotState

object RunRobotScript {
  val smallDimension = Tabletop.dimensions.head
  val largeDimension = Tabletop.dimensions.last

  type RobotScript = PartialFunction[(RobotState, RobotCommand), RobotState]

  def place: RobotScript = {
    case (ValidState(_, _, _, placedObjects), Place(x, y, direction)) => ValidState(x, y, direction, placedObjects)
    case (_, Place(x, y, direction)) => ValidState(x, y, direction, Set())
  }

  def invalidState: RobotScript = {
    case (InvalidState, _) => InvalidState
  }

  def report: RobotScript = {
    case (state@ValidState(x, y, direction, _), Report) => println(s"$x,$y,${direction.toString.toUpperCase}"); state
  }

  def move: RobotScript = {
    case (state@ValidState(_, `largeDimension`, North, _), Move) => state
    case (state@ValidState(`largeDimension`, _, East , _), Move) => state
    case (state@ValidState(_, `smallDimension`, South, _), Move) => state
    case (state@ValidState(`smallDimension`, _, West , _), Move) => state

    case (state@ValidState(x, y, North, placedObjects), Move) if placedObjects.contains(PlacedObject(x, y+1)) => state
    case (state@ValidState(x, y, East , placedObjects), Move) if placedObjects.contains(PlacedObject(x+1, y)) => state
    case (state@ValidState(x, y, South, placedObjects), Move) if placedObjects.contains(PlacedObject(x, y-1)) => state
    case (state@ValidState(x, y, West , placedObjects), Move) if placedObjects.contains(PlacedObject(x-1, y)) => state

    case (ValidState(x, y, North, placedObjects), Move) => ValidState(x, y + 1, North, placedObjects)
    case (ValidState(x, y, East , placedObjects), Move) => ValidState(x + 1, y, East , placedObjects)
    case (ValidState(x, y, South, placedObjects), Move) => ValidState(x, y - 1, South, placedObjects)
    case (ValidState(x, y, West , placedObjects), Move) => ValidState(x - 1, y, West , placedObjects)
  }

  def left: RobotScript = {
    case (ValidState(x, y, North, placedObjects), Left) => ValidState(x, y, West , placedObjects)
    case (ValidState(x, y, East , placedObjects), Left) => ValidState(x, y, North, placedObjects)
    case (ValidState(x, y, South, placedObjects), Left) => ValidState(x, y, East , placedObjects)
    case (ValidState(x, y, West , placedObjects), Left) => ValidState(x, y, South, placedObjects)
  }

  def right: RobotScript = {
    case (ValidState(x, y, North, placedObjects), Right) => ValidState(x, y, East , placedObjects)
    case (ValidState(x, y, East , placedObjects), Right) => ValidState(x, y, South, placedObjects)
    case (ValidState(x, y, South, placedObjects), Right) => ValidState(x, y, West , placedObjects)
    case (ValidState(x, y, West , placedObjects), Right) => ValidState(x, y, North, placedObjects)
  }

  def diagonal: RobotScript = {
    case (state@ValidState(_, _, North, _), Diagonal(South, _)) => state
    case (state@ValidState(_, _, East , _), Diagonal(_, West)) => state
    case (state@ValidState(_, _, South, _), Diagonal(North, _)) => state
    case (state@ValidState(_, _, West , _), Diagonal(_, East)) => state

    case (state@ValidState(_, `largeDimension`, _, _), Diagonal(North, _)) => state
    case (state@ValidState(`largeDimension`, _, _, _), Diagonal(_, East)) => state
    case (state@ValidState(_, `smallDimension`, _, _), Diagonal(South, _)) => state
    case (state@ValidState(`smallDimension`, _, _, _), Diagonal(_, West)) => state

    case (state@ValidState(x, y, direction, placedObjects), Diagonal(North, East)) if placedObjects.contains(PlacedObject(x+1, y+1)) => state
    case (state@ValidState(x, y, direction, placedObjects), Diagonal(South, East)) if placedObjects.contains(PlacedObject(x+1, y-1)) => state
    case (state@ValidState(x, y, direction, placedObjects), Diagonal(South, West)) if placedObjects.contains(PlacedObject(x-1, y-1)) => state
    case (state@ValidState(x, y, direction, placedObjects), Diagonal(North, West)) if placedObjects.contains(PlacedObject(x-1, y+1)) => state

    case (ValidState(x, y, direction, placedObjects), Diagonal(North, East)) => ValidState(x+1, y+1, direction, placedObjects)
    case (ValidState(x, y, direction, placedObjects), Diagonal(South, East)) => ValidState(x+1, y-1, direction, placedObjects)
    case (ValidState(x, y, direction, placedObjects), Diagonal(South, West)) => ValidState(x-1, y-1, direction, placedObjects)
    case (ValidState(x, y, direction, placedObjects), Diagonal(North, West)) => ValidState(x-1, y+1, direction, placedObjects)
  }

  def placeObject: RobotScript = {
    case (state@ValidState(_, `largeDimension`, North, _), PlaceObject) => state
    case (state@ValidState(`largeDimension`, _, East , _), PlaceObject) => state
    case (state@ValidState(_, `smallDimension`, South, _), PlaceObject) => state
    case (state@ValidState(`smallDimension`, _, West , _), PlaceObject) => state

    case (ValidState(x, y, North, placedObjects), PlaceObject) => ValidState(x, y, North, placedObjects + PlacedObject(x, y+1))
    case (ValidState(x, y, East , placedObjects), PlaceObject) => ValidState(x, y, East , placedObjects + PlacedObject(x+1, y))
    case (ValidState(x, y, South, placedObjects), PlaceObject) => ValidState(x, y, South, placedObjects + PlacedObject(x, y-1))
    case (ValidState(x, y, West , placedObjects), PlaceObject) => ValidState(x, y, West , placedObjects + PlacedObject(x-1, y))
  }

  def convertPlacedObjectsToMap(placedObjects: Set[PlacedObject]): String = {
    (smallDimension to largeDimension).map(y => {
      (smallDimension to largeDimension).map(x => {
        placedObjects.contains(PlacedObject(x, y))
      }).map(isPlaced => {
        if (isPlaced) "X" else "0"
      }).reduce((po1, po2) => po1 + po2)
    }).reduce((line1, line2) => line2 + "\n" + line1) + "\n"
  }

  def mapCommand: RobotScript = {
    case (state@ValidState(_, _, _, placedObjects), MapCommand) => println(convertPlacedObjectsToMap(placedObjects)); state
  }

  def nextState(state: RobotState, command: RobotCommand): RobotState = {
    (
      place orElse
      mapCommand orElse
      invalidState orElse
      report orElse
      move orElse
      left orElse
      right orElse
      diagonal orElse
      placeObject
    )(
      (state, command)
    )
  }

  def execute(commands: Iterator[RobotCommand]): RobotState = {
    commands.foldLeft[RobotState](InvalidState)(nextState)
  }
}