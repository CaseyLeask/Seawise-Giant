package InputParsing

import Commands._
import Direction.{Direction, HorizontalDirection, VerticalDirection}

object CommandParsing {
  val placeRegex = "PLACE (\\d+),(\\d)+,(\\w+)".r
  val diagonalRegex = "DIAGONAL (\\w+),(\\w+)".r

  def place(x: String, y: String, direction: String): Option[Place] = {
    Direction.get(direction).map(Place(x.toInt, y.toInt, _))
  }

  def diagonal(first: String, second: String) = {
    for {
      firstDirection <- VerticalDirection.get(first)
      secondDirection <- HorizontalDirection.get(second)
    } yield Diagonal(firstDirection, secondDirection)
  }

  def toRobotCommands(lines: Iterator[String]): Iterator[RobotCommand] = lines.flatMap {
    case placeRegex(x, y, direction) => place(x, y, direction)
    case "MOVE" => Some(Move)
    case "LEFT" => Some(Left)
    case "RIGHT" => Some(Right)
    case "REPORT" => Some(Report)
    case diagonalRegex(firstDirection, secondDirection) => diagonal(firstDirection, secondDirection)
    case "PLACE_OBJECT" => Some(PlaceObject)
    case "MAP" => Some(MapCommand)
    case _ => None
  }
}
