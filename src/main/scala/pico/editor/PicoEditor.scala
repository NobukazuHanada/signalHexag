package hexasignal.pico.editor


import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.control.TextArea
import scalafx.scene.input.{KeyEvent, KeyCode}
import hexasignal.pico.PicoVM


class PicoTextEditor(val vm: PicoVM) extends TextArea("") {
  self =>
  vm.text = text()

  handleEvent(KeyEvent.KeyPressed) {
    (event: KeyEvent) =>
    if( event.code == KeyCode.Enter ){
      vm.text = text()
      text() = vm.text
      self.positionCaret(vm.text.length())
    }
  }

}

import scalafx.scene.control.{TableView, TableColumn}
import scalafx.collections.ObservableBuffer
import scalafx.beans.property.ObjectProperty
import TableColumn._

object PicoValueTable {
  import hexasignal.pico.Pico._
  import hexasignal.pico.Runner._

  type ValueMap = (PicoSymbol, Entity)

  class SymbolTableColumn extends TableColumn[ValueMap, PicoSymbol] {
    text = "Variable Name"
    cellValueFactory = { x => ObjectProperty(x.value._1) }
  }

  class ValueTableColumn extends TableColumn[ValueMap, Entity] {
    text = "Value"
    cellValueFactory = { x => ObjectProperty(x.value._2) }
  }


  class PicoValueTable(val vm: PicoVM) extends TableView[ValueMap](
    ObservableBuffer[ValueMap](vm.getEnv.variableMap.toList:_*)
  ) {
    minWidth = 500
    columns ++= List(new SymbolTableColumn(), new ValueTableColumn())
    vm.addFireEnvEvent {
      env =>
      val valueMaps  : List[ValueMap] = env.variableMap.toList
      items = ObservableBuffer(valueMaps:_*)
      true
    }
  }
}

