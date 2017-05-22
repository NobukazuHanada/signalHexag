package hexasignal.pico.editor


import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.control.TextArea
import scalafx.scene.input.{KeyEvent, KeyCode}
import hexasignal.pico.Environment
import hexasignal.pico.PicoVM

class PicoEditor(env: Environment) extends Scene {
  val textArea = new TextArea("")
  val vm = new PicoVM(env)
  vm.text = textArea.text()

  textArea.handleEvent(KeyEvent.KeyPressed) {
    (event: KeyEvent) =>
    if( event.code == KeyCode.Enter ){
      vm.text = textArea.text()
      textArea.text() = vm.text
      textArea.positionCaret(vm.text.length)
    }
  }

  content = textArea
}
