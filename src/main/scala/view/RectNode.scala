package hexasignal.view

import hexasignal.view.Node
import hexasignal.shape.Hexagon
import hexasignal.shape.Arrow
import hexasignal.message.{Message, RectMessage}
import scalafx.scene.paint.Color._
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.{Pane, StackPane}
import scalafx.scene.{ Group, Node => FxNode }
import scalafx.Includes._
import scalafx.scene.input.{MouseEvent, MouseButton, MouseDragEvent, DragEvent, TransferMode}
import scalafx.beans.property.{DoubleProperty, BooleanProperty}
import javafx.scene.shape.Polygon
import scalafx.geometry.Insets

class RectNode extends
    Node { node =>
  import scalafx.scene.shape.Rectangle
  val rect = new Rectangle() {
    translateX = -5
    translateY = -5
    width = 10
    height = 10
    stroke = White
    fill <== when(hover || pressed) choose rgb(100,100,100,0.8) otherwise rgb(100,100,100,0.1)
  }

  val rectGroup = new Group(rect) {
    handleEvent(MouseEvent.MouseDragged) {
      (mouseEvent:MouseEvent) =>
      val point = node.sceneToLocal(mouseEvent.sceneX, mouseEvent.sceneY)
      translateX() = point.x
      translateY() = point.y
      
    }
  }

  node.children.add(rectGroup)
  
  def message : Message = {
    RectMessage(rectGroup.translateX(),
                rectGroup.translateY(),
                rect.width(),
                rect.height())
  }

}
