package hexasignal.view

import hexasignal.shape.Arrow
import scalafx.scene.layout.Pane
import scalafx.Includes._
import scalafx.scene.input.{MouseEvent, MouseButton, MouseDragEvent}
import MouseButton.{Primary, Secondary}
import akka.actor.{Actor, ActorRef, Props,  ActorSystem}
import hexasignal.model.ViewData
import hexasignal.model.IDGenerator.Id
import akka.pattern.{ask}
import akka.util.Timeout
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Field {
  val actorSystem = ActorSystem.create("syghexActorSystem")
  val actor = actorSystem.actorOf(Props[FieldActor])
  implicit val timeout = new Timeout(5.seconds)
  
  case class CreateActor(props :Props)
  case class ActorOf(actorRef :ActorRef)
  case object GetCommands

  def createActor(props:Props) : Future[ActorRef] = {
    for(ActorOf(actorRef) <- (ask(actor, CreateActor(props))).mapTo[ActorOf])
    yield actorRef
  }

  def terminate {
    actorSystem.terminate()
  }
}

class FieldActor extends Actor {
  import Field._

  import scala.collection.mutable.ListBuffer

  var commands : ListBuffer[Any] = ListBuffer()

  def receive = {
    case CreateActor(props) => sender ! ActorOf(context.actorOf(props))
    case GetCommands =>
      sender ! commands.result()
      commands = ListBuffer()
    case any => commands += any
  }
}

class PlacingField extends Pane {
  placingArea =>
  
  var selectedNode : Option[Node] = None
  var connectors : Map[(Node,Node), Arrow] = Map()
  var nodes : List[Node] = List()

  style = "-fx-border-color: white"
  minWidth = 300
  minHeight = 600

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
      for{
        endActor <- endNode.actor
      } startNode.addSender(endActor)
    }
  }
 
  def sendToNode(data:Map[Id, ViewData]){
    for((id, viewData) <- data){
      val sendData = Data(id, viewData)
      for(((startNode,_), _) <- connectors ){
        startNode.sendToActor(sendData) 
      }
    }
  }

  def createRectMatcherNode(x : Double, y : Double) {
    val node = new RectMatcherNode{
      translateX = x
      translateY = y
    }
    addNode(node)
  }

  def createRectRewriter(x : Double, y : Double) {
    val node = new RectRewriteNode{
      translateX = x
      translateY = y
    }
    addNode(node)
    node.addSender(Field.actor)
  }


  def addNode(node: Node) {
    children.add(node)
    nodes :+= node
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

