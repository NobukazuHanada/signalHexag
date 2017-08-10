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

class CodeNode(val vm: VM) extends HexNode("code") {


  override def receive(m: Message) : Unit = {
    sexprObj() match {
      case Some(sexpr) =>
        send(Message.MCode(vm.sexprTable.map(sexpr), Map()))
      case None =>
    }
  }


  val title = new Label("code inspector")
  title.textFill = Color.White
  val space = new Label("")
  val label = new Label("code")
  label.textFill = Color.White
  val textField = new TextArea()
  //val codeEditor =  new CodeEditor(vm)
  val treeviews = new TreeViews(vm)
  val codeEditorStage = new Stage() {
    width = 500
    height = 500
    scene = new Scene() {
      title = "selected code editor Stage"
      content = new HBox(treeviews)
    }
  }
  textField.onMouseClicked = {
    (event:MouseEvent) =>
    codeEditorStage.show()
  }
  val vbox = new VBox(10, title, space, label, textField)
  vbox.padding = scalafx.geometry.Insets(10,10,10,10)
  textField.text <== treeviews.selectedNodeText
  
  val sexprObj : ObjectProperty[Option[SExprRef[SExpr]]] =
    ObjectProperty(treeviews.selectedNode().map(_.sexpr.ref))

  treeviews.selectedNode.onChange((_,_,newValue) =>
    sexprObj() = treeviews.selectedNode().map(_.sexpr.ref) 
  )



  /*vm.eventsOnRun :+= {
    (vm:VM) =>
    sexprObj().foreach { sexpr =>
      val pos = vm.positionTable.map(sexpr.pos.start).pos
      sexprObj() = vm.sexprTable.search(pos + 1, vm.positionTable)
    }
  }*/
}
