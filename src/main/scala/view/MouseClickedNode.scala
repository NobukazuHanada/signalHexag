package hexasignal.view

import akka.actor.{Actor, ActorRef, Props}
import hexasignal.IDGenerator
import hexasignal.Id
import scalafx.scene.text.{Text, TextAlignment}
import scalafx.scene.paint.Color

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object MouseClicked 

class MouseClickedNode extends Node {
  val actorId : Id = IDGenerator.generate("mouse-clicked-node")
  val actor : Future[ActorRef] = Field.createActor(Props[MouseClickedProcess])
  actor.foreach { actor:ActorRef =>
    Field.clickEventActors = actor :: Field.clickEventActors
  }

  val text = new Text{
    text = "mouseclicked"
    fill = Color.White
    textAlignment = TextAlignment.Center
  }
  children.add(text)
}



class MouseClickedProcess extends Actor {
  var senders : List[ActorRef] = List()

  def receive = {
    case AddSender(sender) => senders :+= sender
    case MouseClicked =>
      senders.foreach(_ ! Bang)
    case _ => 
  }

}
