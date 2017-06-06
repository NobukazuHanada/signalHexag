package hexasignal.view

import scalafx.scene.paint.Color._
import scalafx.scene.{ Group }
import scalafx.Includes._
import scalafx.scene.input.MouseEvent
import scalafx.scene.shape.Shape
import akka.actor.{ActorSystem, Props, Actor, ActorRef}
import hexasignal.model.{ViewData, Rect}
import hexasignal.IDGenerator
import hexasignal.Id
import hexasignal.shape.Hexagon
import hexasignal.shape.Hexagon.polygon
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


case class Data(id:Id, data:ViewData)
case class SetMove(x:Int,y:Int,w:Int,h:Int)

class RectMatcherNode extends
    Node { node =>
  import scalafx.scene.shape.Rectangle
  val actorId : Id = IDGenerator.generate("rect-match-node")
  val actor : Future[ActorRef] = Field.createActor(Props[RectMatcher])

  val rect = new Rectangle() {
    translateX = -5
    translateY = -5
    width = 10
    height = 10
    stroke = White
    fill <== when(hover || pressed) choose rgb(100,100,100,0.8) otherwise rgb(100,100,100,0.1)
  }

  val rectGroup = new Group(rect) 
  node.children.add(rectGroup)
}

class RectMatcher extends Actor{
  var senders : List[ActorRef] = List()

  def receive = {
    case AddSender(sender) =>
      senders :+= sender
    case rectData@Data(id, rect@Rect(_,_,_,_,_,_)) =>
      senders.foreach(sender => sender ! rectData)
  }
}

class RectRewriteNode
    extends Node { node => 
  import scalafx.scene.shape.Rectangle
  val actorId : Id = IDGenerator.generate("rect-match-node")
  val actor : Future[ActorRef] = Field.createActor(Props[RectRewriter])

  val rect = new Rectangle() {
    translateX = -5
    translateY = -5
    width = 10
    height = 10
    stroke = White
    fill <== when(hover || pressed) choose rgb(100,100,100,0.8) otherwise rgb(100,100,100,0.1)
  }

  val smallHexagon : Shape = new Hexagon(polygon)
  smallHexagon.scaleX() = 0.9
  smallHexagon.scaleY() = 0.9

  val rectGroup = new Group(rect) {
    handleEvent(MouseEvent.MouseDragged) {
      (mouseEvent:MouseEvent) =>
      val point = node.sceneToLocal(mouseEvent.sceneX, mouseEvent.sceneY)
      translateX() = point.x
      translateY() = point.y
      val future = actor.map(_ ! SetMove(translateX().toInt, translateY().toInt, 0, 0))
    }
  }

  node.hexagon.strokeWidth = 5.0

  node.children.add(rectGroup)

}

class RectRewriter extends Actor {
  var senders : List[ActorRef] = List()
  var moveX = 0
  var moveY = 0
  var moveW = 0
  var moveH = 0

  def receive = {
    case AddSender(sender) => senders :+= sender
    case SetMove(x,y,w,h) =>
      moveX = x
      moveY = y
      moveW = w
      moveH = h
    case Data(id, rect@Rect(x,y,w,h,fill, stroke)) =>
      val newRect = Rect(x+moveX, y+moveY, w+moveW, h+moveH, fill, stroke)
      senders.foreach(sender=>sender ! Data(id, newRect))
  }
}
