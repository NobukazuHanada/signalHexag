package hexasignal.shape;

import scalafx.scene.shape.{Polygon, Line}
import scalafx.scene.paint.Color._
import scalafx.scene.{Group}
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.geometry.Point2D
import hexasignal.view.ConnectView

class Arrow  (_startX :  Double,
              _startY : Double,
              _endX : Double,
              _endY : Double) extends Group with ConnectView {

  def this() = this(0,0,0,0)

  val line = Line(_startX, _startY, _endX, _endY)
  val startX = line.startX
  val startY = line.startY
  val endX = line.endX
  val endY = line.endY
  mouseTransparent = true

  line.stroke = White
  

  val triangle = Polygon()
  triangle.fill = White



  children.addAll(line, triangle)
}
