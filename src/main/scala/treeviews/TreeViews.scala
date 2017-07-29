package hexasignal.treeviews

import scalafx.Includes._
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.beans.property.DoubleProperty
import scalafx.scene.layout.Pane
import scalafx.scene.Group
import scalafx.scene.input.{MouseEvent, MouseButton, MouseDragEvent}
import MouseButton.{Primary, Secondary}
import hexasignal.natsuki.VM
import hexasignal.natsuki.SyntaxTree
import hexasignal.view.DragMoving



class TreeViews(val vm : VM) extends Pane { treeview =>

  import hexasignal.natsuki._

  val unitW = 10
  val unitH = 10

  




  class SyntaxTreeNode(x : Double, y : Double, var syntax : SyntaxTree) extends Group
      with DragMoving {


    syntax match {
      case True(expr) =>
        val text = new Text(10, 10, "true")
        children.add(text)
      case False(expr) =>
        val text = new Text(10, 10, "false")
        children.add(text)
      case Num(expr) =>
        val text = new Text(10, 10, expr.toString())
        children.add(text)
      case Str(s) =>
        val text = new Text(10, 10, "\"" + s.content + "\"")
        children.add(text)
      case Symbol(s) =>
        val text = new Text(10, 10, s.name)
        children.add(text)
      case Var(v) =>
        val text = new Text(10, 10, v.name)
        children.add(text)
      case ConsList(list, expr) =>
        val nodes =
          for((syntaxTree, i) <- list.zipWithIndex)
          yield {
            val childX = x + i * 30
            val childY = y + 30
            new SyntaxTreeNode(childX, childY, syntaxTree)
          }
      case _ =>
        
    }





    handleEvent(MouseEvent.DragDetected)
    { (event: MouseEvent) =>
      startFullDrag()
    }

    handleEvent(MouseEvent.MousePressed) {
      (event: MouseEvent) =>

      event.button match {
        case Primary => moveStart()
        case Secondary =>
        case _ =>
      }
    }

    handleEvent(MouseDragEvent.MouseDragEntered){
      (event: MouseEvent) =>
    }

    handleEvent(MouseDragEvent.MouseDragExited){
      (event: MouseEvent) =>
    }

    handleEvent(MouseDragEvent.MouseDragReleased){
      (event: MouseEvent) =>
    }

    handleEvent(MouseEvent.MouseDragged) {
      (event: MouseEvent) =>
      val pos = treeview.sceneToLocal(event.sceneX,event.sceneY)
      moving(pos.x, pos.y)
    }

    handleEvent(MouseEvent.MouseReleased) {
      (event: MouseEvent) =>
      moveEnd()
    }


  }


}


