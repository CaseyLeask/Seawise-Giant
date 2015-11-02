package InputParsing

sealed trait Direction
case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction

sealed trait RobotCommand
case class Place(x: Int, y: Int, direction: Direction) extends RobotCommand
case object Move extends RobotCommand
case object Left extends RobotCommand
case object Right extends RobotCommand
case object Report extends RobotCommand

object Direction {
  val directionMapping = Map[String, Direction](
    North.toString.toUpperCase -> North,
    East.toString.toUpperCase -> East,
    South.toString.toUpperCase -> South,
    West.toString.toUpperCase -> West
  )
  def get = directionMapping.get _
}

object Tabletop {
  def dimensions = 0 to 4
}

object CommandParsing {
  val place = "PLACE (\\d+),(\\d)+,(\\w+)".r

  def coordinatesOnTable(x: String, y: String): Boolean = {
    Tabletop.dimensions.contains(x.toInt) && Tabletop.dimensions.contains(y.toInt)
  }

  def toRobotCommands(lines: Iterator[String]): Iterator[RobotCommand] = lines.flatMap {
    case place(x, y, direction) if coordinatesOnTable(x, y) =>
      Direction.get(direction).map(Place(x.toInt, y.toInt, _))
    case "MOVE" => Some(Move)
    case "LEFT" => Some(Left)
    case "RIGHT" => Some(Right)
    case "REPORT" => Some(Report)
    case _ => None
  }
}
