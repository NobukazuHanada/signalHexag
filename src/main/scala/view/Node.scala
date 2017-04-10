package hexasignal.view

import hexasignal.shape.Hexagon
import hexasignal.shape.Hexagon.polygon
import hexasignal.shape.Arrow
import scalafx.scene.shape.Shape
import scalafx.scene.paint.Color._
import scalafx.scene.{ Group, Node => FxNode }
import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, BooleanProperty}
import scala.collection.mutable.ArrayBuffer
import akka.actor.ActorRef

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

  def connectStart(viewGroups: PlacingField ){
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

  def connectEnd(viewGroups: PlacingField){
    connecting = false
    viewGroups.children.remove(connectorView)
  }
}


case class AddSender(sender:ActorRef)
trait ActorController {

  val actor : ActorRef
  def addSender(sender:ActorRef) : Unit = {
    actor ! AddSender(sender)
  }
}

trait Node
    extends Group
    with DragMoving
    with ConnectionNode[Arrow]
    with ActorController {
  node =>

  val from : ArrayBuffer[Node] = ArrayBuffer.empty[Node]
  val to : ArrayBuffer[Node] = ArrayBuffer.empty[Node]

  def addToNode(node: Node) {
    from += node
    node.to += this
  }

  def deleteToNode(node: Node) {
    from -= node
    node.to -= this
  }

  val hexagon : Shape = new Hexagon(polygon)
  children.add(hexagon)

  val connectorView = new Arrow()
  val dragEventHover =  BooleanProperty(false)

  hexagon.fill <== when(hover || pressed || dragEventHover) choose rgb(100,100,100,0.5) otherwise rgb(100,100,100,0.1)

}
