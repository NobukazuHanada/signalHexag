package com.nobkz.hexagonSingal.view;

import com.nobkz.hexagonSingal.shape.Hexagon
import com.nobkz.hexagonSingal.shape.Arrow
import scalafx.scene.paint.Color._
import scalafx.scene.layout.Pane
import scalafx.scene.{ Node => FxNode } 
import scalafx.Includes._
import scalafx.scene.input.{MouseEvent, MouseButton, MouseDragEvent, DragEvent, TransferMode}
import scalafx.beans.property.{DoubleProperty, BooleanProperty}
import javafx.scene.shape.Polygon
import scalafx.geometry.Insets

trait DragMoving extends FxNode {
  
  var moving = false

  def moveStart() {
    moving = true
  }

  def moving(x : Double, y: Double) {
    if( moving ){
      translateX() = x
      translateY() = y
    }
  }

  def moveEnd(){
    moving = false
  }
}

trait ConnectView extends FxNode {
  val startX : DoubleProperty
  val startY : DoubleProperty
  val endX : DoubleProperty
  val endY : DoubleProperty
}


trait ConnectionNode[A <: ConnectView] extends FxNode {
  var connecting = false
  val connectorView : A
  val viewGroups : PlacingField


  def connectStart(){
    connectorView.startX() = translateX()
    connectorView.startY() = translateY()
    connectorView.endX() = translateX()
    connectorView.endY() = translateY()
    connecting = true
    viewGroups.children.add(connectorView)
  }

  def connecterMove(x : Double, y : Double){
    connectorView.endX() = x
    connectorView.endY() = y
  }

  def connectEnd(){
    connecting = false
    viewGroups.children.remove(connectorView)
  }
}

class PlacingField extends Pane {
  placingArea =>

  import scala.collection.mutable.Map
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


  import com.nobkz.hexagonSingal.shape.Hexagon.polygon

  def createNode(x : Double, y : Double) {
    val node = new Node(polygon, this, new Arrow(x,y,x,y)){
      translateX = x
      translateY = y
    }

    children.add(node)
  }



  class Node (polygon : Polygon, group: PlacingField, arrow: Arrow)
      extends Hexagon(polygon)
      with DragMoving
      with ConnectionNode[Arrow] {


    val connectorView = arrow
    val viewGroups = group
    val dragEventHover =  BooleanProperty(false)


    fill <== when(hover || pressed || dragEventHover) choose rgb(100,100,100,0.5) otherwise rgb(100,100,100,0.1)


    handleEvent(MouseEvent.DragDetected)
    { (event: MouseEvent) =>
      startFullDrag()
    }

    handleEvent(MouseEvent.MousePressed) {
      (event: MouseEvent) =>
      import scalafx.scene.input.MouseButton.{Primary, Secondary}

      selectedNode = Some(this)
      event.button match {
        case Primary => moveStart()
        case Secondary =>
          connectStart()
        case _ =>
          selectedNode = None
      }
    }


    

    handleEvent(MouseDragEvent.MouseDragEntered){
      (event: MouseEvent) =>
      if( selectedNode.nonEmpty && !selectedNode.contains(this) ){
        dragEventHover() = true
      }
    }

    handleEvent(MouseDragEvent.MouseDragExited){
      (event: MouseEvent) =>
      dragEventHover() = false
    }


    handleEvent(MouseDragEvent.MouseDragReleased){
      (event: MouseEvent) =>
      for( n <- selectedNode if n != this ){
        placingArea.connect(n, this)
      }
    }



    handleEvent(MouseEvent.MouseDragged) {
      (event: MouseEvent) =>
      val pos = placingArea.sceneToLocal(event.sceneX,event.sceneY)
      if( moving ){
        moving(pos.x, pos.y)
      }else if( connecting ){
        connecterMove(pos.x,pos.y)
      }
    }

    handleEvent(MouseEvent.MouseReleased) {
      (event: MouseEvent) =>
      mouseTransparent = false
      selectedNode = None

      if( moving ){
        moveEnd()
      }else if( connecting ){
        connectEnd()
      }
    }
  }
}
