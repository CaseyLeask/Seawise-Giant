package InputParsing

sealed trait Direction
case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction

sealed trait RobotCommands
case class Place(x: Int, y: Int, direction: Direction) extends RobotCommands
case object Move extends RobotCommands
case object Left extends RobotCommands
case object Right extends RobotCommands
case object Report extends RobotCommands

object Direction {
  val directionMapping = Map[String, Direction](
    North.toString.toUpperCase -> North,
    East.toString.toUpperCase -> East,
    South.toString.toUpperCase -> South,
    West.toString.toUpperCase -> West
  )
  def get = directionMapping.get _
}

object CommandParsing {
  val place = "PLACE (\\d+),(\\d)+,(\\w+)".r
  def toRobotCommands(lines: Iterator[String]): Iterator[RobotCommands] = lines.flatMap {
    case place(x, y, direction) =>
      Direction.get(direction).map(Place(x.toInt, y.toInt, _))
    case "MOVE" => Some(Move)
    case "LEFT" => Some(Left)
    case "RIGHT" => Some(Right)
    case "REPORT" => Some(Report)
    case _ => None
  }
}
