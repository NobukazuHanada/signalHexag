package hexasignal.natsuki.editor

import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.control.TextArea
import scalafx.scene.input.{KeyEvent, KeyCode}
import scalafx.scene.input.MouseEvent
import hexasignal.natsuki.VM
import hexasignal.natsuki.PositionRef
import hexasignal.natsuki.Position
import hexasignal.natsuki.PositionTable

class CodeEditor(val vm: VM) extends TextArea("") {
  self =>

  minHeight = 600

  text() = vm.text()
  vm.text <==> this.text

  handleEvent(MouseEvent.MouseClicked) {
    (e:MouseEvent) =>
    vm.run(text())
  }

  vm.addEventsOnChangeSExpr {
    case (ref, oldSExpr, newSExpr) =>
      import hexasignal.natsuki.Atom._
        (oldSExpr, newSExpr) match {
        case (Sym(oldString,_,_), Sym(newString,_,_)) =>
          changeText(oldSExpr.pos.start, oldString, newString)
        case (Var(oldString,_,_), Var(newString,_,_)) =>
          changeText(oldSExpr.pos.start, oldString, newString)
        case (Str(oldString,_,_), Str(newString,_,_)) =>
          changeText(oldSExpr.pos.start, oldString, newString)
        case (Num.I(oldI,_,_), Num.I(newI,_,_)) =>
          changeText(oldSExpr.pos.start, oldI.toString(), newI.toString())
        case (Num.D(oldD,_,_), Num.D(newD,_,_)) =>
          changeText(oldSExpr.pos.start, oldD.toString(), newD.toString())
        case (Num.R(oldNR,oldDR,_,_), Num.R(newNR,newDR,_,_)) =>
          changeText(oldSExpr.pos.start,
                     oldNR.toString() + "/" + oldDR.toString(),
                     newNR.toString() + "/" + newDR.toString())
        case (Bool(oldB,_,_), Bool(newB,_,_)) =>
          changeText(oldSExpr.pos.start, oldB.toString(), newB.toString())
        case _=>
      }
  }

  def changeText(posRef: PositionRef, oldString:String, newString:String) : Unit = {
    val position = vm.positionTable.map(posRef)
    this.deleteText(position.pos, position.pos + oldString.length())
    this.insertText(position.pos, newString)

    val newPositionMap = vm.positionTable.map.map {
      case (ref, pos) =>
        if( pos.pos > position.pos ) {
          (ref, Position(pos.pos + (newString.length() - oldString.length())
             ,ref))
        }else{
          (ref,pos)
        }
    }
    vm.positionTable = PositionTable(newPositionMap)
  }

}
