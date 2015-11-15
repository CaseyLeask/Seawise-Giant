package State

import Commands._
import Direction.Direction
import Tabletop._

sealed trait RobotState
case object InvalidState extends RobotState
case class ValidState(x: Int, y: Int, direction: Direction, placedObjects: Set[PlacedObject]) extends RobotState

case class RobotResult(state: RobotState, action: Option[String] = None)

object RobotState {
  type RobotScript = PartialFunction[(RobotState, RobotCommand), RobotResult]

  def place: RobotScript = {
    case (ValidState(_, _, _, placedObjects), Place(x, y, direction)) if Tabletop.validPlace(x, y, placedObjects) =>
      RobotResult(ValidState(x, y, direction, placedObjects))
    case (InvalidState, Place(x, y, direction)) if Tabletop.validPlace(x, y, Set()) => RobotResult(ValidState(x, y, direction, Set()))
    case (state, Place(_, _, _)) => RobotResult(state)
  }

  def invalidState: RobotScript = {
    case (InvalidState, _) => RobotResult(InvalidState)
  }

  def report: RobotScript = {
    case (state@ValidState(x, y, direction, _), Report) => RobotResult(state, Some(s"$x,$y,${direction.toString.toUpperCase}"))
  }

  def move: RobotScript = {
    case (ValidState(x, y, d, placedObjects), Move) if Tabletop.validPlace(x + d.dx, y + d.dy, placedObjects) =>
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
    case (ValidState(x, y, direction, placedObjects), Diagonal(dv, dh)) if Tabletop.validPlace(x + dh.dx, y + dv.dy, placedObjects) && (direction == dv || direction == dh) =>
      RobotResult(ValidState(x+dh.dx, y+dv.dy, direction, placedObjects))

    case (state, Diagonal(_, _)) => RobotResult(state)
  }

  def placeObject: RobotScript = {
    case (ValidState(x, y, d, placedObjects), PlaceObject) if Tabletop.validPlace(x + d.dx, y + d.dy, placedObjects) =>
      RobotResult(ValidState(x, y, d, placedObjects + PlacedObject(x+d.dx, y+d.dy)))

    case (state, PlaceObject) => RobotResult(state)
  }

  def mapCommand: RobotScript = {
    case (state@ValidState(_, _, _, placedObjects), MapCommand) => RobotResult(state, Some(Tabletop.convertPlacedObjectsToMap(placedObjects)))
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
}