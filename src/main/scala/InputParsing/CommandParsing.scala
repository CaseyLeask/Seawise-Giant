package InputParsing

import Commands._
import Direction.{Direction, HorizontalDirection, VerticalDirection}


object Tabletop {
  def dimensions = 0 to 4
}

object CommandParsing {
  val placeRegex = "PLACE (\\d+),(\\d)+,(\\w+)".r
  val diagonalRegex = "DIAGONAL (\\w+),(\\w+)".r

  def coordinatesOnTable(x: String, y: String): Boolean = {
    Tabletop.dimensions.contains(x.toInt) && Tabletop.dimensions.contains(y.toInt)
  }

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
    case placeRegex(x, y, direction) if coordinatesOnTable(x, y) =>
      place(x, y, direction)
    case "MOVE" => Some(Move)
    case "LEFT" => Some(Left)
    case "RIGHT" => Some(Right)
    case "REPORT" => Some(Report)
    case diag@diagonalRegex(firstDirection, secondDirection) =>
      diagonal(firstDirection, secondDirection)
    case "PLACE_OBJECT" => Some(PlaceObject)
    case "MAP" => Some(MapCommand)
    case _ => None
  }
}
