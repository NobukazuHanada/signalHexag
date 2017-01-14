package hexasignal.model

import hexasignal.model.{Model, Watcher}
import scalafx.scene.paint.Color
import scalafx.scene.canvas.Canvas
import scalafx.scene.shape.{Rectangle, Line => FxLine, Polygon}
import scalafx.scene.Scene
import scalafx.scene.Group
import scalafx.scene.Node
import scalafx.stage.Stage

sealed abstract class ViewData
case class Rect(x : Double, y : Double, w : Double , h : Double, fill : Color, stroke : Color ) extends ViewData
case class Line(startX : Double, startY : Double, endX : Double, endY : Double, color : Color) extends ViewData


class ViewModel extends Model {
  import scala.collection.mutable.ListBuffer
  var dataModel : ListBuffer[ViewData] = ListBuffer[ViewData]()

  def rect(x : Double, y : Double, w : Double, h : Double, fill : Color, stroke : Color) {
    dataModel += Rect(x,y,w,h,fill,stroke)
    notice()
  }

  def line(x1 : Double, y1 : Double, x2 : Double, y2 : Double, c : Color) {
    dataModel += Line(x1,y1,x2,y2,c)
    notice()
  }

  def updateModel(matcher : ViewData => Boolean, update : ViewData => ViewData) {
    dataModel =
      for( viewData <- dataModel )
      yield if( matcher(viewData) ){
        update(viewData)
      }else{
        viewData
      }
    notice()
  }

  def modelMatch(matcher : ViewData => Boolean) : List[ViewData] =
    dataModel.filter(matcher).toList

  def toSeq() : Seq[ViewData] = dataModel.toSeq

}

class ViewModelRenderer(val model : ViewModel) extends Scene
    with Watcher {

  val canvas = new Canvas(500, 500)
  val gc = canvas.graphicsContext2D
  gc.fill = Color.Black
  model.addWatcher(this)

  noticed()
  def noticed() {
    gc.fillRect(0, 0, 500, 500)
    for( viewData <- model.dataModel )
      viewData match {
        case Rect(x,y,w,h,fill,stroke) =>
          val newX = x % 500
          val newY = y % 500
          gc.fill = fill
          gc.stroke = stroke
          gc.fillRect(newX,newY,w,h)
          gc.strokeRect(newX,newY,w,h)
        case Line(x1,y1,x2,y2,c) =>
          gc.stroke = c
          gc.beginPath()
          gc.moveTo(x1, y1)
          gc.lineTo(x2, y2)
          gc.closePath()
      }
  }

  content = canvas
}
