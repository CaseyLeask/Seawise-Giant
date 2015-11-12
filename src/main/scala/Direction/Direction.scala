package Direction

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