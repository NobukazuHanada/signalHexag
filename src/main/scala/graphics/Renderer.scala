package hexasignal.graphics

import scalafx.scene.paint.{Color => FxColor}
import scalafx.scene.canvas.Canvas
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scalafx.Includes._

trait EventReceiver {
  def receiveEvent()
}

class Renderer extends Scene {
  val canvas = new Canvas(500, 500)
  val gc = canvas.graphicsContext2D
  var eventReceiver : Option[EventReceiver] = Option.empty

  gc.setFill(FxColor.Black)
  gc.clearRect(0, 0, 500, 500)
  gc.fillRect(0,0,500,500)

  canvas.handleEvent(MouseEvent.MouseClicked) {
    (event:MouseEvent) =>
    eventReceiver.foreach(_.receiveEvent())
  }

  content = canvas
}
