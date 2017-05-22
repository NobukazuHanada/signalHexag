package hexasignal.pico.editor


import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.control.TextArea
import scalafx.scene.input.{KeyEvent, KeyCode}
import hexasignal.pico.PicoParser

class PicoEditor extends Scene {
  val textArea = new TextArea("")

  


  content = textArea
}
