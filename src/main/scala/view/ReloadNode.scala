package hexasignal.view

import akka.actor.{Actor, ActorRef, Props}
import hexasignal.IDGenerator
import hexasignal.Id
import scala.concurrent.Future
import scalafx.scene.text.Text
import scalafx.scene.paint.Color
import scalafx.Includes._

object Reload

class ReloadNode extends Node {
  val actorId : Id = IDGenerator.generate("reload-node")
  val actor : Future[ActorRef] = Field.createActor(Props[ReloadProcess])

  val text = new Text{
    text = "reload"
    fill = Color.White
  }
  children.add(text)
}

class ReloadProcess extends Actor{
  var senders : List[ActorRef] = List()

  def receive = {
    case AddSender(sender) => senders :+= sender
    case Bang =>
      senders.foreach(_ ! Reload)
  }
}
