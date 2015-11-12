package Commands

import Direction._

sealed trait RobotCommand
case class Place(x: Int, y: Int, direction: Direction) extends RobotCommand
case object Move extends RobotCommand
case object Left extends RobotCommand
case object Right extends RobotCommand
case object Report extends RobotCommand
case class Diagonal(verticalDirection: VerticalDirection, horizontalDirection: HorizontalDirection) extends RobotCommand
case object PlaceObject extends RobotCommand
case object MapCommand extends RobotCommand