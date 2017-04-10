package hexasignal.view

import scalafx.scene.paint.Color._
import scalafx.scene.{ Group }
import scalafx.Includes._
import scalafx.scene.input.MouseEvent
import akka.actor.{ActorSystem, Props, Actor, ActorRef}
import hexasignal.model.{ViewData, Rect}
import hexasignal.model.IDGenerator.Id

case class Data(id:Id, data:ViewData)
case class SetMove(x:Int,y:Int,w:Int,h:Int)

class RectMatcherNode(val actorSystem : ActorSystem) extends
    Node { node =>
  import scalafx.scene.shape.Rectangle
  val actor = actorSystem.actorOf(Props[RectMatcher])

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
    case AddSender(sender) => senders :+= sender
    case rectData@Data(id, Rect(_,_,_,_,_,_)) =>
      senders.foreach(sender => sender ! rectData)
  }
}

class RectRewriteNode(val actorSystem : ActorSystem)
    extends Node { node => 
  import scalafx.scene.shape.Rectangle

  val actor = actorSystem.actorOf(Props[RectRewriter])

  val rect = new Rectangle() {
    translateX = -5
    translateY = -5
    width = 10
    height = 10
    stroke = White
    fill <== when(hover || pressed) choose rgb(100,100,100,0.8) otherwise rgb(100,100,100,0.1)
  }

  val rectGroup = new Group(rect) {
    handleEvent(MouseEvent.MouseDragged) {
      (mouseEvent:MouseEvent) =>
      val point = node.sceneToLocal(mouseEvent.sceneX, mouseEvent.sceneY)
      translateX() = point.x
      translateY() = point.y

      actor ! SetMove(translateX().toInt, translateY().toInt, 0, 0)
    }
  }
  node.children.add(rectGroup)


  

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
      case Data(id, Rect(x,y,w,h,fill, stroke)) =>
        val newRect = Rect(x+moveX, y+moveY, w+moveW, h+moveH, fill, stroke)
        senders.foreach(sender=>sender ! Data(id, newRect))
    }
  }

}
