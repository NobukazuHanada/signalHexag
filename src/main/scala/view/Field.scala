package hexasignal.view

import hexasignal.shape.Hexagon
import hexasignal.shape.Arrow
import hexasignal.message.{Message, RectMessage}
import hexasignal.model.{ViewData, Rect}
import scalafx.scene.paint.Color._
import scalafx.scene.layout.Pane
import scalafx.scene.{ Group, Node => FxNode }
import scalafx.Includes._
import scalafx.scene.input.{MouseEvent, MouseButton, MouseDragEvent, DragEvent, TransferMode}
import scala.collection.mutable.Map


class PlacingField extends Pane {
  placingArea =>
  var selectedNode : Option[Node] = None
  val connectors : Map[(Node,Node), Arrow] = Map()

  def connect(startNode : Node, endNode : Node) {
    val arrow = new Arrow()
    if( !(connectors contains (startNode -> endNode)) ){
         arrow.startX <== startNode.translateX
         arrow.startY <== startNode.translateY
         arrow.endX <== endNode.translateX
         arrow.endY <== endNode.translateY
         children.add(arrow)
         connectors += (startNode -> endNode) -> arrow
       }
  }

  style = "-fx-border-color: white"
  minWidth = 300
  minHeight = 600

  def createRectNode(x : Double, y : Double) {
    import hexasignal.shape.Hexagon.polygon
    val node = new RectNode(){
      translateX = x
      translateY = y
    }

    addNode(node)
  }


  def addNode(node: Node) {
    children.add(node)
    node.hexagon.handleEvent(MouseEvent.DragDetected)
    { (event: MouseEvent) =>
      startFullDrag()
    }

    node.hexagon.handleEvent(MouseEvent.MousePressed) {
      (event: MouseEvent) =>
      import scalafx.scene.input.MouseButton.{Primary, Secondary}

      selectedNode = Some(node)
      event.button match {
        case Primary => node.moveStart()
        case Secondary =>
          node.connectStart(placingArea)
        case _ =>
          selectedNode = None
      }
    }

    node.hexagon.handleEvent(MouseDragEvent.MouseDragEntered){
      (event: MouseEvent) =>
      if( selectedNode.nonEmpty && !selectedNode.contains(this) ){
        node.dragEventHover() = true
      }
    }

    node.hexagon.handleEvent(MouseDragEvent.MouseDragExited){
      (event: MouseEvent) =>
      node.dragEventHover() = false
    }

    node.hexagon.handleEvent(MouseDragEvent.MouseDragReleased){
      (event: MouseEvent) =>
      for( n <- selectedNode if n != node ){
        placingArea.connect(n, node)
      }
    }



    node.handleEvent(MouseEvent.MouseDragged) {
      (event: MouseEvent) =>
      val pos = placingArea.sceneToLocal(event.sceneX,event.sceneY)
      if( node.moving ){
        node.moving(pos.x, pos.y)
      }else if( node.connecting ){
        node.connecterMove(pos.x,pos.y)
      }
    }

    node.handleEvent(MouseEvent.MouseReleased) {
      (event: MouseEvent) =>
      node.mouseTransparent = false
      selectedNode = None

      if( node.moving ){
        node.moveEnd()
      }else if( node.connecting ){
        node.connectEnd(placingArea)
      }
    }
  }


  def updater : ViewData => ViewData  = {
    var updater : ViewData => ViewData = (x)=>x

    for(((node1, node2), _) <- connectors ){
      (node1.message, node2.message) match {
        case (RectMessage(x1,y1,w1,h1),RectMessage(x2,y2,w2,h2)) =>
          val diffX = (x2 - x1) / 10
          val diffY = (y2 - y1) / 10
          val diffW = (w2 - w1) / 10
          val diffH = (h2 - h1) / 10
          updater = {
             case Rect(x,y,w,h,f,s) =>
               Rect(x+diffX,y+diffY,w+diffW,h+diffH,f,s)
             case x => x
          }
        case _ =>
      }
    }
    return updater
  }
 

}

