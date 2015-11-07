package CommandExecution

import InputParsing._

case class PlacedObject(x: Int, y: Int)

sealed trait RobotState
case object InvalidState extends RobotState
case class ValidState(x: Int, y: Int, direction: Direction, placedObjects: Set[PlacedObject]) extends RobotState

case class RobotResult[A](state: RobotState, action: A = Unit)

object RunRobotScript {
  val smallDimension = Tabletop.dimensions.head
  val largeDimension = Tabletop.dimensions.last

  type RobotScript[A] = PartialFunction[(RobotState, RobotCommand), RobotResult[A]]

  def place: RobotScript[Unit] = {
    case (ValidState(_, _, _, placedObjects), Place(x, y, direction)) => RobotResult(ValidState(x, y, direction, placedObjects))
    case (_, Place(x, y, direction)) => RobotResult(ValidState(x, y, direction, Set()))
  }

  def invalidState: RobotScript[Unit] = {
    case (InvalidState, _) => RobotResult(InvalidState)
  }

  def report: RobotScript[String] = {
    case (state@ValidState(x, y, direction, _), Report) => RobotResult(state, s"$x,$y,${direction.toString.toUpperCase}")
  }

  def move: RobotScript[Unit] = {
    case (state@ValidState(_, `largeDimension`, North, _), Move) => RobotResult(state)
    case (state@ValidState(`largeDimension`, _, East , _), Move) => RobotResult(state)
    case (state@ValidState(_, `smallDimension`, South, _), Move) => RobotResult(state)
    case (state@ValidState(`smallDimension`, _, West , _), Move) => RobotResult(state)

    case (state@ValidState(x, y, North, placedObjects), Move) if placedObjects.contains(PlacedObject(x, y+1)) => RobotResult(state)
    case (state@ValidState(x, y, East , placedObjects), Move) if placedObjects.contains(PlacedObject(x+1, y)) => RobotResult(state)
    case (state@ValidState(x, y, South, placedObjects), Move) if placedObjects.contains(PlacedObject(x, y-1)) => RobotResult(state)
    case (state@ValidState(x, y, West , placedObjects), Move) if placedObjects.contains(PlacedObject(x-1, y)) => RobotResult(state)

    case (ValidState(x, y, North, placedObjects), Move) => RobotResult(ValidState(x, y + 1, North, placedObjects))
    case (ValidState(x, y, East , placedObjects), Move) => RobotResult(ValidState(x + 1, y, East , placedObjects))
    case (ValidState(x, y, South, placedObjects), Move) => RobotResult(ValidState(x, y - 1, South, placedObjects))
    case (ValidState(x, y, West , placedObjects), Move) => RobotResult(ValidState(x - 1, y, West , placedObjects))
  }

  def left: RobotScript[Unit] = {
    case (ValidState(x, y, North, placedObjects), Left) => RobotResult(ValidState(x, y, West , placedObjects))
    case (ValidState(x, y, East , placedObjects), Left) => RobotResult(ValidState(x, y, North, placedObjects))
    case (ValidState(x, y, South, placedObjects), Left) => RobotResult(ValidState(x, y, East , placedObjects))
    case (ValidState(x, y, West , placedObjects), Left) => RobotResult(ValidState(x, y, South, placedObjects))
  }

  def right: RobotScript[Unit] = {
    case (ValidState(x, y, North, placedObjects), Right) => RobotResult(ValidState(x, y, East , placedObjects))
    case (ValidState(x, y, East , placedObjects), Right) => RobotResult(ValidState(x, y, South, placedObjects))
    case (ValidState(x, y, South, placedObjects), Right) => RobotResult(ValidState(x, y, West , placedObjects))
    case (ValidState(x, y, West , placedObjects), Right) => RobotResult(ValidState(x, y, North, placedObjects))
  }

  def diagonal: RobotScript[Unit] = {
    case (state@ValidState(_, _, North, _), Diagonal(South, _)) => RobotResult(state)
    case (state@ValidState(_, _, East , _), Diagonal(_, West)) =>  RobotResult(state)
    case (state@ValidState(_, _, South, _), Diagonal(North, _)) => RobotResult(state)
    case (state@ValidState(_, _, West , _), Diagonal(_, East)) =>  RobotResult(state)

    case (state@ValidState(_, `largeDimension`, _, _), Diagonal(North, _)) => RobotResult(state)
    case (state@ValidState(`largeDimension`, _, _, _), Diagonal(_, East)) =>  RobotResult(state)
    case (state@ValidState(_, `smallDimension`, _, _), Diagonal(South, _)) => RobotResult(state)
    case (state@ValidState(`smallDimension`, _, _, _), Diagonal(_, West)) =>  RobotResult(state)

    case (state@ValidState(x, y, direction, placedObjects), Diagonal(North, East)) if placedObjects.contains(PlacedObject(x+1, y+1)) => RobotResult(state)
    case (state@ValidState(x, y, direction, placedObjects), Diagonal(South, East)) if placedObjects.contains(PlacedObject(x+1, y-1)) => RobotResult(state)
    case (state@ValidState(x, y, direction, placedObjects), Diagonal(South, West)) if placedObjects.contains(PlacedObject(x-1, y-1)) => RobotResult(state)
    case (state@ValidState(x, y, direction, placedObjects), Diagonal(North, West)) if placedObjects.contains(PlacedObject(x-1, y+1)) => RobotResult(state)

    case (ValidState(x, y, direction, placedObjects), Diagonal(North, East)) => RobotResult(ValidState(x+1, y+1, direction, placedObjects))
    case (ValidState(x, y, direction, placedObjects), Diagonal(South, East)) => RobotResult(ValidState(x+1, y-1, direction, placedObjects))
    case (ValidState(x, y, direction, placedObjects), Diagonal(South, West)) => RobotResult(ValidState(x-1, y-1, direction, placedObjects))
    case (ValidState(x, y, direction, placedObjects), Diagonal(North, West)) => RobotResult(ValidState(x-1, y+1, direction, placedObjects))
  }

  def placeObject: RobotScript[Unit] = {
    case (state@ValidState(_, `largeDimension`, North, _), PlaceObject) => RobotResult(state)
    case (state@ValidState(`largeDimension`, _, East , _), PlaceObject) => RobotResult(state)
    case (state@ValidState(_, `smallDimension`, South, _), PlaceObject) => RobotResult(state)
    case (state@ValidState(`smallDimension`, _, West , _), PlaceObject) => RobotResult(state)

    case (ValidState(x, y, North, placedObjects), PlaceObject) => RobotResult(ValidState(x, y, North, placedObjects + PlacedObject(x, y+1)))
    case (ValidState(x, y, East , placedObjects), PlaceObject) => RobotResult(ValidState(x, y, East , placedObjects + PlacedObject(x+1, y)))
    case (ValidState(x, y, South, placedObjects), PlaceObject) => RobotResult(ValidState(x, y, South, placedObjects + PlacedObject(x, y-1)))
    case (ValidState(x, y, West , placedObjects), PlaceObject) => RobotResult(ValidState(x, y, West , placedObjects + PlacedObject(x-1, y)))
  }

  def convertPlacedObjectsToMap(placedObjects: Set[PlacedObject]): String = {
    (smallDimension to largeDimension).map(y => {
      (smallDimension to largeDimension).map(x => {
        placedObjects.contains(PlacedObject(x, y))
      }).map(isPlaced => {
        if (isPlaced) "X" else "0"
      }).reduce((po1, po2) => po1 + po2)
    }).reduce((line1, line2) => line2 + "\n" + line1)
  }

  def mapCommand: RobotScript[String] = {
    case (state@ValidState(_, _, _, placedObjects), MapCommand) => RobotResult(state, convertPlacedObjectsToMap(placedObjects))
  }

  def nextResult(state: RobotState, command: RobotCommand): RobotResult[_] = {
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

  def nextState(result: RobotResult[_]): RobotState = {
    if (result.action != ()) println(result.action.toString)

    result.state
  }

  def execute(commands: Iterator[RobotCommand]): RobotState = {
    commands.foldLeft[RobotState](InvalidState)((state, command) => {
      nextState(nextResult(state, command))
    })
  }
}
