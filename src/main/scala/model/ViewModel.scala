package hexasignal.model

import scalafx.scene.paint.Color
import scalafx.scene.canvas.Canvas
import scalafx.scene.Scene

sealed abstract class ViewData
case class Rect(x : Double, y : Double, w : Double , h : Double, fill : Color, stroke : Color ) extends ViewData
case class Line(startX : Double, startY : Double, endX : Double, endY : Double, color : Color) extends ViewData
case class BrokenExpr(s:String) extends ViewData
case object Undefined extends ViewData

object IDGenerator{
  type Id = String
  val hostname = java.net.InetAddress.getLocalHost().getHostName()
  var counter = 0

  def generate = {
    val now = java.time.Instant.now().toString()
    counter += 1;
    val text = hostname + now + counter.toString()
    java.security.MessageDigest.getInstance("MD5").digest(text.getBytes).map("%02x".format(_)).mkString
  }
  
  def generate(text :String) = {
    val now = java.time.Instant.now().toString()
    counter += 1;
    val phrase = text + hostname + now + counter.toString() 
    java.security.MessageDigest.getInstance("MD5").digest(phrase.getBytes).map("%02x".format(_)).mkString
  }
}

class ViewModel extends Model {
  import IDGenerator.Id
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
