package hexasignal.view

import akka.actor.{Actor, ActorRef, Props}
import hexasignal.IDGenerator
import hexasignal.Id
import scalafx.Includes._
import scalafx.scene.text.{Text, TextAlignment}
import scalafx.scene.paint.Color
import scalafx.scene.input.MouseEvent
import scalafx.scene.Scene
import scalafx.stage.Stage
import scalafx.scene.layout.{
  HBox,
  VBox
}
import scalafx.scene.control.{
  TextField,
  TextArea,
  Label
}
import scalafx.beans.property.{
  ObjectProperty
}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.parsing.input.CharSequenceReader
import hexasignal.natsuki.editor.CodeEditor
import hexasignal.natsuki.{VM,
  FilterParser,
  SExpr,
  SExprRef
}
import hexasignal.treeviews.TreeViews

class SimpleActor extends Actor {
  def receive  = {
    case _ =>
  }
}

abstract class HexNode(name: String) extends Node {
  val actorId : Id = IDGenerator.generate("mouse-clicked-node")
  val actor : Future[ActorRef] = Field.createActor(Props[SimpleActor])

  val text = new Text{
    translateX = - 15
    translateY = 2.5
    text = name
    fill = Color.White
    textAlignment = TextAlignment.Center
    alignmentInParent = scalafx.geometry.Pos.Center
  }
  children.add(text)
}


class MetroNode extends HexNode("metro")

class ClickNode extends HexNode("click")

class KeyNode extends HexNode("key")

class WatchNode(val vm: VM) extends HexNode("watch")

class DelayNode extends HexNode("delay") {
  val title = new Label("delay inspector")
  title.textFill = Color.White
  val space = new Label("")
  val label = new Label("delay")
  label.textFill = Color.White
  val textField = new TextField()
  val vbox = new VBox(10, title, space, label, textField)
  vbox.padding = scalafx.geometry.Insets(10,10,10,10)


  override def receive(m:Message) : Unit = {
    val str = textField.text()
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toInt match {
      case Some(i) =>
        import scala.concurrent.Future
        val s = Future {
          Thread.sleep(i)
          send(m)
        }
      case _ => 
    }
  }
}


