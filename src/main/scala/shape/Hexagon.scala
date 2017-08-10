package hexasignal.shape;

import scalafx.Includes._
import scalafx.collections.{ObservableBuffer}
import scalafx.beans.property.{ObjectProperty}
import scalafx.scene.shape.Polygon
import scalafx.scene.input.{MouseEvent, MouseButton, MouseDragEvent}
import scalafx.scene.paint.Color._
import scalafx.scene.Group

object Hexagon {
  val radius = 50
  val radiusHalf = radius / 2
  val hexaWidth = radiusHalf * Math.sqrt(3)
  val centerX : Double = 0;
  val centerY : Double = 0;

  val points = ObservableBuffer[Double](
    centerX, centerY + radius,
    centerX - hexaWidth, centerY + radiusHalf,
    centerX - hexaWidth, centerY - radiusHalf,
    centerX, centerY - radius,
    centerX + hexaWidth, centerY - radiusHalf,
    centerX + hexaWidth, centerY + radiusHalf
  )

  

  var aim = false
  var move = false
  var source : Option[Hexagon] = None
  var target : Option[Hexagon] = None
  var newArrow : Option[Arrow] = None
  var group = new Group();
  import scala.collection.mutable.Set
  val connectArrows : Set[Arrow] = Set()

  def apply(x:Double, y:Double) : Hexagon  = 
    new Hexagon(new javafx.scene.shape.Polygon(points:_*)) {
      translateX = x
      translateY = y
    }

  def polygon : javafx.scene.shape.Polygon = {
    val p = new javafx.scene.shape.Polygon(points:_*)
    p.rotate = 90
    p
  }

}



class Hexagon(polygon:javafx.scene.shape.Polygon) extends Polygon(polygon) {

  stroke = White
  strokeWidth = 1

}
