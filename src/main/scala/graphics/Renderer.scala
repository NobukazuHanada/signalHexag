package hexasignal.graphics

import scalafx.scene.paint.{Color => FxColor}
import scalafx.scene.canvas.Canvas
import scalafx.scene.Scene

class Renderer extends Scene {
  val canvas = new Canvas(500, 500)
  val gc = canvas.graphicsContext2D
  gc.setFill(FxColor.Black)
  gc.clearRect(0, 0, 500, 500)
  gc.fillRect(0,0,500,500)

  content = canvas
}
