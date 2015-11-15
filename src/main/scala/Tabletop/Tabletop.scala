package Tabletop

case class PlacedObject(x: Int, y: Int)

object Tabletop {
  def dimensions = 0 to 4

  def convertPlacedObjectsToMap(placedObjects: Set[PlacedObject]): String = {
    dimensions.map(y => {
      dimensions.map(x => {
        placedObjects.contains(PlacedObject(x, y))
      }).map(isPlaced => {
        if (isPlaced) "X" else "0"
      }).reduce((po1, po2) => po1 + po2)
    }).reduce((line1, line2) => line2 + "\n" + line1)
  }

  def validPlace(x: Int, y: Int, placedObjects: Set[PlacedObject]): Boolean = {
    dimensions.contains(x) &&
      dimensions.contains(y) &&
      !placedObjects.contains(PlacedObject(x, y))
  }
}