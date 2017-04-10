package hexasignal.view

import hexasignal.shape.Arrow
import scalafx.scene.layout.Pane
import scalafx.Includes._
import scalafx.scene.input.{MouseEvent, MouseButton, MouseDragEvent}
import MouseButton.{Primary, Secondary}
import scala.collection.mutable.Map
import akka.actor.ActorSystem


class PlacingField(val actorSystem:ActorSystem) extends Pane {
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
      startNode.addToNode(endNode)
    }
  }

  style = "-fx-border-color: white"
  minWidth = 300
  minHeight = 600

  def createRectMatcherNode(x : Double, y : Double) {
    val node = new RectMatcherNode(actorSystem){
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

}

