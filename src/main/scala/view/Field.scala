package hexasignal.view

import hexasignal.shape.Arrow
import scalafx.scene.layout.Pane
import scalafx.Includes._
import scalafx.scene.input.{MouseEvent, MouseButton, MouseDragEvent}
import MouseButton.{Primary, Secondary}
import akka.actor.{Actor, ActorRef, Props,  ActorSystem}
import hexasignal.model.ViewData
import hexasignal.Id
import hexasignal.graphics.EventReceiver
import akka.actor.{ActorRef}
import akka.pattern.{ask}
import akka.util.Timeout
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Field extends  EventReceiver{
  val actorSystem = ActorSystem.create("syghexActorSystem")
  val actor = actorSystem.actorOf(Props[FieldActor])
  var clickEventActors : List[ActorRef] = List[ActorRef]()
  implicit val timeout = new Timeout(5.seconds)
  
  case class CreateActor(props :Props)
  case class ActorOf(actorRef :ActorRef)
  case object GetCommands

  def receiveEvent() {
    clickEventActors.foreach(ask(_,MouseClicked))
  }

  def createActor(props:Props) : Future[ActorRef] = {
    for(ActorOf(actorRef) <- (ask(actor, CreateActor(props))).mapTo[ActorOf])
    yield actorRef
  }

  def terminate = 
    actorSystem.terminate()
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


  var move = false
  var vectors : List[Vector2] = List()
  handleEvent(MouseEvent.MouseMoved) {
    event: MouseEvent =>
    if( move )
      vectors :+= Vector2(event.screenX, event.screenY)
  }

  handleEvent(MouseEvent.MouseClicked) {
    (mouseEvent: MouseEvent) =>
    if( move == false ){
      move = true
    }else{
      move = false
      GestureParser.parse(vectors) match {
        case Some(t) => println(t)
        case _ =>
      }
      vectors = List()

    }
    if( mouseEvent.clickCount == 2) {
      import scalafx.stage.Stage
      import scalafx.scene.Scene
      import scalafx.scene.layout.HBox
      import scalafx.scene.control.TextField
      import scalafx.scene.control.Button
     

      val stage = new Stage(){ stage =>
        scene = new Scene(){
          content = new HBox(){
            val textField = new TextField()
            val button = new Button("create!"){
              val clock = """clock (\d+)""".r
              onAction = {
                event =>
                textField.text() match {
                  case "mouseclicked" =>
                    val node = new MouseClickedNode() {
                      translateX = mouseEvent.x
                      translateY = mouseEvent.y
                    }
                    placingArea.addNode(node)
                    stage.close()
                  case "reload" =>
                    val node = new ReloadNode() {
                      translateX = mouseEvent.x
                      translateY = mouseEvent.y
                    }
                    placingArea.addNode(node)
                    node.addSender(Field.actor)
                    stage.close()
                  case clock(clockTime) =>
                    try {
                      val node = new ClockNode(clockTime.toInt) {
                        translateX = mouseEvent.x
                        translateY = mouseEvent.y
                      }
                      placingArea.addNode(node)
                    } catch {
                      case _ =>
                    }
                    stage.close()
                  case _ =>
                    
                }
              }
            }
            children = Seq(textField, button)
          }
        }
      }
      stage.show()
    }
  }
}
