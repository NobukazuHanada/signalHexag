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

class ReloadNode(val vm: VM) extends HexNode("reload") {

  override def receive(m:Message) : Unit = {
    m match {
      case Message.MCode(expr, pos) =>
        vm.eval(expr)
      case _ =>

    }
  }
  
}
