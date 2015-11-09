package CommandExecution

import InputParsing._

case class PlacedObject(x: Int, y: Int)

sealed trait RobotState
case object InvalidState extends RobotState
case class ValidState(x: Int, y: Int, direction: Direction, placedObjects: Set[PlacedObject]) extends RobotState

case class RobotResult(state: RobotState, action: Option[String] = None)

object RunRobotScript {
  val smallDimension = Tabletop.dimensions.head
  val largeDimension = Tabletop.dimensions.last

  type RobotScript = PartialFunction[(RobotState, RobotCommand), RobotResult]

  def onTheBoard(x: Int, y: Int): Boolean = {
    Tabletop.dimensions.contains(x) && Tabletop.dimensions.contains(y)
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

  def place: RobotScript = {
    case (state@ValidState(_, _, _, placedObjects), Place(x, y, _)) if placedObjects.contains(PlacedObject(x, y)) =>
      RobotResult(state)
    case (ValidState(_, _, _, placedObjects), Place(x, y, direction)) => RobotResult(ValidState(x, y, direction, placedObjects))
    case (_, Place(x, y, direction)) => RobotResult(ValidState(x, y, direction, Set()))
  }

  def invalidState: RobotScript = {
    case (InvalidState, _) => RobotResult(InvalidState)
  }

  def report: RobotScript = {
    case (state@ValidState(x, y, direction, _), Report) => RobotResult(state, Some(s"$x,$y,${direction.toString.toUpperCase}"))
  }

  def move: RobotScript = {
    case (state@ValidState(x, y, d, placedObjects), Move) if placedObjects.contains(PlacedObject(x + d.dx, y+ d.dy)) =>
      RobotResult(state)

    case (ValidState(x, y, d, placedObjects), Move) if onTheBoard(x + d.dx, y + d.dy) =>
      RobotResult(ValidState(x + d.dx, y + d.dy, d, placedObjects))

    case (state, Move) => RobotResult(state)
  }

  def left: RobotScript = {
    case (ValidState(x, y, d, placedObjects), Left) => RobotResult(ValidState(x, y, d.counterclockwise, placedObjects))
  }

  def right: RobotScript = {
    case (ValidState(x, y, d, placedObjects), Right) => RobotResult(ValidState(x, y, d.clockwise, placedObjects))
  }

  def diagonal: RobotScript = {
    case (state@ValidState(x, y, direction, placedObjects), Diagonal(dv, dh)) if placedObjects.contains(PlacedObject(x+dh.dx, y+dv.dy)) =>
      RobotResult(state)

    case (ValidState(x, y, direction, placedObjects), Diagonal(dv, dh)) if onTheBoard(x + dh.dx, y + dv.dy) && (direction == dv || direction == dh) =>
      RobotResult(ValidState(x+dh.dx, y+dv.dy, direction, placedObjects))

    case (state, Diagonal(_, _)) => RobotResult(state)
  }

  def placeObject: RobotScript = {
    case (ValidState(x, y, d, placedObjects), PlaceObject) if onTheBoard(x + d.dx, y + d.dy) =>
      RobotResult(ValidState(x, y, d, placedObjects + PlacedObject(x+d.dx, y+d.dy)))

    case (state, PlaceObject) => RobotResult(state)
  }

  def mapCommand: RobotScript = {
    case (state@ValidState(_, _, _, placedObjects), MapCommand) => RobotResult(state, Some(convertPlacedObjectsToMap(placedObjects)))
  }

  def nextResult(state: RobotState, command: RobotCommand): RobotResult = {
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

  def nextState(result: RobotResult): RobotState = result match {
    case RobotResult(state, Some(action)) => println(action); state
    case RobotResult(state, None) => state
  }

  def execute(commands: Iterator[RobotCommand]): RobotState = {
    commands.foldLeft[RobotState](InvalidState)((state, command) => {
      nextState(nextResult(state, command))
    })
  }
}
