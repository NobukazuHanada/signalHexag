package hexasignal.model

import scalafx.scene.paint.Color
import scalafx.scene.canvas.Canvas
import scalafx.scene.Scene
import hexasignal.Id
import hexasignal.IDGenerator

sealed abstract class ViewData
case class Rect(x : Double, y : Double, w : Double , h : Double, fill : Color, stroke : Color ) extends ViewData
case class Line(startX : Double, startY : Double, endX : Double, endY : Double, color : Color) extends ViewData
case class BrokenExpr(s:String) extends ViewData
case object Undefined extends ViewData


class ViewModel extends Model {
  var dataModel : Map[Id, ViewData] = Map()

  def update(id:Id, viewData:ViewData) {
    dataModel += (id -> viewData)
    notice()
  }

  def setDataModel(data : Map[Id, ViewData]) {
    dataModel = data
    notice()
  }

}

class ViewModelRenderer(val model : ViewModel) extends Scene
    with Watcher {

  val canvas = new Canvas(500, 500)
  val gc = canvas.graphicsContext2D
  gc.fill = Color.Black
  gc.clearRect(0, 0, 500, 500)
  model.addWatcher(this)

  noticed() 
  def noticed() {
    gc.fill = Color.Black
    gc.fillRect(0, 0, 500, 500)
    for( (id, viewData) <- model.dataModel )
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
          gc.strokePath()
        case _ =>
      }
  }

  content = canvas
}
