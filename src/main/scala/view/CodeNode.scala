package hexasignal.view

import hexasignal.view.Node
import hexasignal.shape.Hexagon
import hexasignal.shape.Arrow
import hexasignal.message.{Message, CodeMessage, CodeCompiler}
import scalafx.scene.paint.Color._
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.{Pane, StackPane}
import scalafx.scene.{ Group, Node => FxNode }
import scalafx.Includes._
import scalafx.scene.input.{MouseEvent, MouseButton, MouseDragEvent, DragEvent, TransferMode}
import scalafx.beans.property.{DoubleProperty, BooleanProperty}
import javafx.scene.shape.Polygon
import scalafx.geometry.Insets
import scalafx.scene.control.TextArea
import scalafx.scene.layout.Background


class CodeNode extends
    Node { node =>

  val textArea = new TextArea() {
    translateX = -30
    translateY = -30
    maxWidth = 60
    maxHeight = 60
    style = "-fx-control-inner-background:#000000; -fx-highlight-fill: #00ff00; -fx-highlight-text-fill: #000000; -fx-text-fill: #ffffff;"
  }

  node.children.add(textArea)
  
  def message : Message = {
    CodeMessage(CodeCompiler.read(textArea.text()))
  }

}
