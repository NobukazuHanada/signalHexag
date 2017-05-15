package hexasignal.view

import hexasignal.message.{Message, CodeMessage, CodeCompiler}
import scalafx.scene.control.TextArea
import akka.actor.{Actor, ActorSystem, Props, ActorRef}



class CodeNode extends
    Node { node =>

  val actor = Field.createActor(Props[CodeReader])

  val textArea = new TextArea() {
    translateX = -30
    translateY = -30
    maxWidth = 60
    maxHeight = 60
    style = "-fx-control-inner-background:#000000; -fx-highlight-fill: #00ff00; -fx-highlight-text-fill: #000000; -fx-text-fill: #ffffff;"
  }

  node.children.add(textArea)
  
  def message : Message = {
    CodeMessage(CodeCompiler.read(textArea.text()))
  }

  class CodeReader extends Actor{
    var senders : List[ActorRef] = List()

    def receive = {
      case AddSender(sender) => senders :+= sender
    }
  }

}
