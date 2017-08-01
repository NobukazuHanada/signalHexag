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

  handleEvent(MouseEvent.MouseClicked) {
    (e:MouseEvent) =>
    vm.run(text())
  }

  vm.addEventsOnChangeSExpr {
    case (ref, oldSExpr, newSExpr) =>
      import hexasignal.natsuki.Atom._
        (oldSExpr, newSExpr) match {
        case (Sym(oldString,_,_), Sym(newString,_,_)) =>
          changeText(oldSExpr.pos, oldString, newString)
        case (Var(oldString,_,_), Var(newString,_,_)) =>
          changeText(oldSExpr.pos, oldString, newString)
        case (Str(oldString,_,_), Str(newString,_,_)) =>
          changeText(oldSExpr.pos, oldString, newString)
        case (Num.I(oldI,_,_), Num.I(newI,_,_)) =>
          changeText(oldSExpr.pos, oldI.toString(), newI.toString())
        case (Num.D(oldD,_,_), Num.D(newD,_,_)) =>
          changeText(oldSExpr.pos, oldD.toString(), newD.toString())
        case (Num.R(oldNR,oldDR,_,_), Num.R(newNR,newDR,_,_)) =>
          changeText(oldSExpr.pos,
                     oldNR.toString() + "/" + oldDR.toString(),
                     newNR.toString() + "/" + newDR.toString())
        case (Bool(oldB,_,_), Bool(newB,_,_)) =>
          changeText(oldSExpr.pos, oldB.toString(), newB.toString())
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
