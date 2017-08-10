package hexasignal

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.stage.Stage
import scalafx.scene.{Scene, Group, Node => FxNode}
import scalafx.animation.{Timeline, KeyValue, KeyFrame, Animation, AnimationTimer}
import scalafx.scene.layout.{HBox, BorderPane}
import scalafx.scene.transform.{Translate}
import scalafx.scene.shape.Sphere
import scalafx.scene.PerspectiveCamera
import scalafx.scene.paint.Color._
import scalafx.stage.WindowEvent
import scalafx.scene.input.MouseEvent
import hexasignal.view.PlacingField
import hexasignal.view.Field
import hexasignal.sound.Sound

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import akka.pattern.ask

import akka.util.Timeout
import scala.concurrent.duration._


object Main extends JFXApp {
  import hexasignal.natsuki.editor.CodeEditor
  import hexasignal.natsuki.VM
  import hexasignal.treeviews.TreeViews

  val vm = new VM
  val field = new PlacingField(vm)

  stage = new JFXApp.PrimaryStage { stage =>
    title.value = "Hexagon Signal Editor"
    width = 930
    height = 730

    scene = new Scene {
      fill = rgb(193,193,193)

      val borderPane = new BorderPane(field, null, field.inspectorView, null, null)
      content = borderPane
      borderPane.prefWidth = 930
      borderPane.prefHeight = 730

      import scalafx.stage.WindowEvent

      handleEvent(WindowEvent.WindowCloseRequest) {
        (event:WindowEvent) =>
        val future = for {
          _ <- Sound.quit
          f <- Field.terminate
        } yield f
      }
    }
  }

  val codeEditor =  new CodeEditor(vm)
  val treeviews = new TreeViews(vm)
  val codeEditorStage = new Stage() {
    width = 1000
    height = 500
    scene = new Scene() {
      title = "natsuki code editor Stage"
      content = new HBox(codeEditor, treeviews)
    }
  }

  codeEditorStage.show()

  /*import hexasignal.pico.editor.PicoTextEditor
  import hexasignal.pico.editor.PicoValueTable.PicoValueTable
  import hexasignal.pico.PicoVM
  import hexasignal.pico.Runner._
  import hexasignal.pico.Environment
  import hexasignal.pico.library.Standard
  import hexasignal.graphics.GraphicFunctions
  import hexasignal.graphics.Drawer
  import hexasignal.graphics.Renderer
  import hexasignal.pico.editor.PicoASTEditor
  val renderer = new Renderer()
  val rendererStage = new Stage {
    width = 500
    height = 500
    title = "renderer Stage"
    scene = renderer
  }
  rendererStage.show
  val drawer = new Drawer(renderer.gc)

  var env = Environment()
  case class Rect(x:Int, y:Int, w:Int, h:Int)
  case class Line(x1:Int, y1:Int, x2:Int, y2:Int)
  case class SinOsc(f:Int)
  env = env.addForeignFunc("rect") {
    case Seq(EntInt(x), EntInt(y), EntInt(w), EntInt(h))
        => Rect(x,y,w,h) }
  env = env.addForeignFunc("line") {
    case Seq(EntInt(x), EntInt(y), EntInt(w), EntInt(h))
        => Line(x,y,w,h)
  }
  env = env.addForeignFunc("sinOsc") { case Seq(EntInt(f)) => SinOsc(f) }
  env = env.addForeignFunc("+")(Standard.add)
  env = env.addForeignFunc("-")(Standard.sub)
  env = env.addForeignFunc("*")(Standard.mul)
  env = env.addForeignFunc("/")(Standard.div)
  env = env.addForeignFunc("=")(Standard.eq)
  env = env.addForeignFunc("not")(Standard.not)
  env = env.addForeignFunc("first")(Standard.first)
  env = env.addForeignFunc("rest")(Standard.rest)
  env = env.addForeignFunc("cons")(Standard.cons)
  env = env.addForeignFunc("rect")(GraphicFunctions.rect)
  env = env.addForeignFunc("line")(GraphicFunctions.line)
  env = env.addForeignFunc("oval")(GraphicFunctions.oval)
  env = env.addForeignFunc("group")(GraphicFunctions.group)
  env = env.addForeignFunc("rgba")(GraphicFunctions.rgba)
  env = env.addForeignFunc("hsba")(GraphicFunctions.hsba)
  env = env.addForeignFunc("draw")(drawer.draw)
  env = env.addForeignFunc("sinOsc")(Sound.sinOsc)
  env = env.addForeignFunc("play")(Sound.play)
  env = env.addForeignFunc("perc")(Sound.perc)
  env = env.addForeignFunc("toy*")(Sound.mul)


  val picoVM = new PicoVM(env)
  val picoTextEditor = new PicoTextEditor(picoVM)
  val picoASTEditor = new PicoASTEditor(picoVM)
  val picoEditorStage = new Stage{
    title = "pico editor"
    scene = new Scene(){
      content = new HBox() {
        children = Seq(
          picoTextEditor,
          picoASTEditor
        )
      }
    }
  }
  picoEditorStage.show()

  val picoValueTableStage = new Stage{
    title = "value table"
    scene = new Scene(){ content = new PicoValueTable(picoVM) }
  }
  picoValueTableStage.show()


  renderer.eventReceiver = Some(Field)
  implicit val timeout = new Timeout(600.seconds)
  def askLoop : Future[Unit] = {
    ask(Field.actor, Field.GetCommands).mapTo[List[Any]].map {
      commands => commands map {
        case hexasignal.view.Reload =>
          picoTextEditor.reload
      }
    }
  } flatMap(_=> askLoop)
  val future = askLoop */
}
