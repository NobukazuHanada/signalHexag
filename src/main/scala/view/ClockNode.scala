package hexasignal.view


import akka.actor.{Actor, ActorRef, Props}
import hexasignal.IDGenerator
import hexasignal.Id
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalafx.scene.text.Text
import scalafx.scene.paint.Color

case class Schedule(interval: Int)

class ClockNode(val interval: Int) extends Node {
  val actorId : Id = IDGenerator.generate("reload-node")
  val actor : Future[ActorRef] = Field.createActor(Props[ClockProcess])
  actor.map(_ ! Schedule(interval))

  val text = new Text{
    text = s"clock $interval"
    fill = Color.White
  }
  children.add(text)
}


class ClockProcess extends Actor{
  import scala.concurrent.duration._
  var senders : List[ActorRef] = List()

  def receive = {
    case AddSender(sender) => senders :+= sender
    case Bang =>
      senders.foreach(_ ! Bang)
    case Schedule(interval) =>
      context.system.scheduler.schedule(interval millis, interval millis, self, Bang)
  }
}
