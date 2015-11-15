package Tabletop

case class PlacedObject(x: Int, y: Int)

object Tabletop {
  def dimensions = 0 to 4

  val smallDimension = Tabletop.dimensions.head
  val largeDimension = Tabletop.dimensions.last

  def convertPlacedObjectsToMap(placedObjects: Set[PlacedObject]): String = {
    (smallDimension to largeDimension).map(y => {
      (smallDimension to largeDimension).map(x => {
        placedObjects.contains(PlacedObject(x, y))
      }).map(isPlaced => {
        if (isPlaced) "X" else "0"
      }).reduce((po1, po2) => po1 + po2)
    }).reduce((line1, line2) => line2 + "\n" + line1)
  }

  def validPlace(x: Int, y: Int, placedObjects: Set[PlacedObject]): Boolean = {
    Tabletop.dimensions.contains(x) &&
      Tabletop.dimensions.contains(y) &&
      !placedObjects.contains(PlacedObject(x, y))
  }
}