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
import hexasignal.natsuki.VM

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

class PlacingField(val vm: VM) extends Pane {
  placingArea =>
  
  var selectedNode : Option[Node] = None
  val inspectorView = new InspectorView()
  var connectors : Map[(Node,Node), Arrow] = Map()
  var nodes : List[Node] = List()

  //style = "-fx-border-color: white"
  minWidth = 300
  minHeight = 600

  def connect(startNode : Node, endNode : Node) {
    val arrow = new Arrow()
    if( !(connectors contains (startNode -> endNode)) ){
      arrow.startX <== startNode.translateX
      arrow.startY <== startNode.translateY
      arrow.endX <== endNode.translateX
      arrow.endY <== endNode.translateY
      children.add(0, arrow)
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
    
    node.hexagon.handleEvent(MouseEvent.MouseClicked)
    { (event: MouseEvent) =>
      inspectorView.inspectedNode = node
      node match {
        case n:ClickNode =>
          n.send(Message.MBang)
        case _ =>
      }
    }

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


  handleEvent(MouseEvent.MouseMoved) {
    event: MouseEvent =>
  }

  handleEvent(MouseEvent.MouseClicked) {
    (mouseEvent: MouseEvent) =>
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
                createNode(textField.text(), mouseEvent)
                stage.close()
              }
            }
            children = Seq(textField, button)
          }
        }
      }
      stage.show()
    }
  }


  def createNode(name:String, mouseEvent: MouseEvent) : Unit = {
    trait Positioned extends Node {
      translateX = mouseEvent.x
      translateY = mouseEvent.y
    }

    name match {
      case "metro" =>
        val node = new MetroNode() with Positioned
        placingArea.addNode(node)
      case "click" =>
        val node = new ClickNode() with Positioned
        placingArea.addNode(node)
      case "key" =>
        val node = new KeyNode() with Positioned
        placingArea.addNode(node)
      case "watch" =>
        val node = new WatchNode(vm) with Positioned
        placingArea.addNode(node)
      case "code" =>
        val node = new CodeNode(vm) with Positioned
        placingArea.addNode(node)
      case "filter" =>
        val node = new FilterNode(vm) with Positioned
        placingArea.addNode(node)
      case "trans" =>
        val node = new TransNode(vm) with Positioned
        placingArea.addNode(node)
      case "reload" =>
        val node = new ReloadNode(vm) with Positioned
        placingArea.addNode(node)
      case "delay" =>
        val node = new DelayNode() with Positioned
        placingArea.addNode(node)
      case _ =>
    }
  }

  class InspectorView extends Pane {
    import scalafx.scene.paint.Color
    style() = "-fx-background-color: white"
    prefWidth = 200
    minHeight() = placingArea.minHeight()
    style = "-fx-background-color: rgb(136,136,136)"

    private var _inspectedNode : Option[Node] = None
    def inspectedNode : Option[Node] = _inspectedNode
    def inspectedNode_=(node:Node): Unit = {
      _inspectedNode = Some(node)
      children.removeAll(children)

      import scalafx.scene.control.{
        TextArea, Label, TextField
      }
      import scalafx.scene.layout.VBox
      import scalafx.geometry.Pos

      val result = node match {
        case node:MetroNode =>
          val title = new Label("metro inspector")
          title.textFill = Color.White
          val space = new Label("")
          val label = new Label("time interval")
          label.textFill = Color.White
          val textField = new TextField()
          val vbox = new VBox(10, title, space, label, textField)
          vbox.padding = scalafx.geometry.Insets(10,10,10,10)
          children.add(vbox)

        case node:ClickNode =>
          val title = new Label("click inspector")
          title.textFill = Color.White
          val vbox = new VBox(10, title)
          vbox.padding = scalafx.geometry.Insets(10,10,10,10)
          children.add(vbox)


        case node:KeyNode =>
          val title = new Label("key inspector")
          title.textFill = Color.White
          val vbox = new VBox(10, title)
          vbox.padding = scalafx.geometry.Insets(10,10,10,10)
          children.add(vbox)


        case node:WatchNode =>
          val title = new Label("watch inspector")
          title.textFill = Color.White
          val space = new Label("")
          val label = new Label("code")
          label.textFill = Color.White
          val textField = new TextArea()
          val vbox = new VBox(10, title, space, label, textField)
          vbox.padding = scalafx.geometry.Insets(10,10,10,10)
          children.add(vbox)

        case node:CodeNode =>
          children.add(node.vbox)

        case filter:FilterNode =>
          children.add(filter.vbox)

        case node:TransNode =>
          children.add(node.vbox)


        case node:ReloadNode =>
          val title = new Label("reload inspector")
          title.textFill = Color.White
          val vbox = new VBox(10, title)
          vbox.padding = scalafx.geometry.Insets(10,10,10,10)
          children.add(vbox)

        case node:DelayNode =>
          children.add(node.vbox)
      }
    }

  }


}
