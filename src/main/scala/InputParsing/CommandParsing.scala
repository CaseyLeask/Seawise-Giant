package InputParsing

sealed trait Direction {
  def dx: Int
  def dy: Int
  def clockwise: Direction
  def counterclockwise: Direction
}

abstract sealed class VerticalDirection(override val dy: Int) extends Direction {
  override val dx: Int = 0
}

abstract sealed class HorizontalDirection(override val dx: Int) extends Direction {
  override val dy: Int = 0
}

case object North extends VerticalDirection  ( 1) {
  override def clockwise: Direction = East
  override def counterclockwise: Direction = West
}

case object East  extends HorizontalDirection( 1) {
  override def clockwise: Direction = South
  override def counterclockwise: Direction = North
}

case object South extends VerticalDirection  (-1) {
  override def clockwise: Direction = West
  override def counterclockwise: Direction = East
}

case object West extends HorizontalDirection(-1) {
  override def clockwise: Direction = North
  override def counterclockwise: Direction = South
}

sealed trait RobotCommand
case class Place(x: Int, y: Int, direction: Direction) extends RobotCommand
case object Move extends RobotCommand
case object Left extends RobotCommand
case object Right extends RobotCommand
case object Report extends RobotCommand
case class Diagonal(verticalDirection: VerticalDirection, horizontalDirection: HorizontalDirection) extends RobotCommand
case object PlaceObject extends RobotCommand
case object MapCommand extends RobotCommand

object VerticalDirection {
  val directionMapping = Map[String, VerticalDirection](
    North.toString.toUpperCase -> North,
    South.toString.toUpperCase -> South
  )

  def get = directionMapping.get _
}

object HorizontalDirection {
  val directionMapping = Map[String, HorizontalDirection](
    East.toString.toUpperCase -> East,
    West.toString.toUpperCase -> West
  )
  def get = directionMapping.get _
}

object Direction {
  def get(direction: String): Option[Direction] = {
    VerticalDirection.get(direction) orElse HorizontalDirection.get(direction)
  }
}

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
