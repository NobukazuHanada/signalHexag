package hexasignal

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.stage.Stage
import scalafx.scene.{Scene, Group, Node => FxNode}
import scalafx.animation.{Timeline, KeyValue, KeyFrame, Animation, AnimationTimer}
import scalafx.scene.layout.HBox
import scalafx.scene.transform.{Translate}
import scalafx.scene.shape.Sphere
import scalafx.scene.PerspectiveCamera
import scalafx.scene.paint.Color._
import scalafx.stage.WindowEvent
import hexasignal.view.PlacingField
import hexasignal.view.Field
import de.sciss.synth._
import ugen._
import Ops._

/*
import akka.actor.{Actor}
import akka.event.Logging

class MyActor extends Actor {
  val log = Logging(context.system, this)
  var stage2 : Stage = null

  def receive = {
    case "window" =>
    case "test" =>
      log.info("info test")
    case _ => log.info("received unknown message")
  }
}
 */
 

object Main extends JFXApp {
  val field = new PlacingField()

  stage = new JFXApp.PrimaryStage {
    title.value = "Hexagon Signal Editor"
    width = 930
    height = 730

    scene = new Scene {
      fill = Black



      field.createRectMatcherNode(50.0 + 70.0, 10.0)
      field.createRectRewriter(50.0 + 80.0, 20.0)

      content = new HBox(field) {
        translateX = 10
        translateY = 10
      }
      import scalafx.stage.WindowEvent
      handleEvent(WindowEvent.WindowCloseRequest) {
        (event:WindowEvent) =>
        val future = Field.terminate
      }
    }
  }


  /*val cfg = Server.Config()
  cfg.program = "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/scsynth"

  Server.run(cfg) { s =>
    val sin = SynthDef("Sin") {
      val f1 = "freq1".kr(100)
      SinOsc.ar(f1)
    }

    
   }*/


  import hexasignal.model.{ViewModel, ViewModelRenderer
  ,ViewModelEditor}
  
  val viewModel = new ViewModel()
  val viewStage = new Stage(){
    title() = "Hexagon Signal View"

    width = 500
    height = 500

    val vmr = new ViewModelRenderer(viewModel)
    scene = vmr

    val animationTimer = AnimationTimer(
      (time) => {
        val dataModel = viewModel.dataModel
        field.sendToNode(dataModel)
      }
    )
    animationTimer.start()
    import akka.pattern.ask
    import scala.collection.mutable.ListBuffer
    import Field.timeout
    import hexasignal.view.Data
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future

    def askLoop : Future[Unit] = {
      ask(Field.actor, Field.GetCommands).mapTo[List[Any]].map {
        commands => commands map {
          case Data(id, data) =>
            println(data)
            viewModel(id) = data
        }
      }
    } flatMap(_=> askLoop)
    val future = askLoop
  }

  //  viewStage.show()
  // viewStage.toFront()

  val viewModelEditorStage = new Stage{
    val vme = new ViewModelEditor(viewModel);
    scene = vme
  }

  //viewModelEditorStage.show();

  import hexasignal.pico.editor.PicoTextEditor
  import hexasignal.pico.editor.PicoValueTable.PicoValueTable
  import hexasignal.pico.PicoVM
  import hexasignal.pico.Runner._
  import hexasignal.pico.Environment
  import hexasignal.pico.library.Standard
  import hexasignal.graphics.GraphicFunctions
  import hexasignal.graphics.Drawer
  import hexasignal.graphics.Renderer
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
  env = env.addForeignFunc("rect")(Standard.rest)
  env = env.addForeignFunc("cons")(Standard.cons)
  env = env.addForeignFunc("rect")(GraphicFunctions.rect)
  env = env.addForeignFunc("line")(GraphicFunctions.line)
  env = env.addForeignFunc("oval")(GraphicFunctions.oval)
  env = env.addForeignFunc("group")(GraphicFunctions.group)
  env = env.addForeignFunc("rgba")(GraphicFunctions.rgba)
  env = env.addForeignFunc("hsba")(GraphicFunctions.hsba)
  env = env.addForeignFunc("draw")(drawer.draw)

  val picoVM = new PicoVM(env)
  val picoEditorStage = new Stage{
    title = "pico editor"
    scene = new Scene(){ content = new PicoTextEditor(picoVM) } 
  }
  picoEditorStage.show()

  val picoValueTableStage = new Stage{
    title = "value table"
    scene = new Scene(){ content = new PicoValueTable(picoVM) }
  }
  picoValueTableStage.show()

  val picoASTEditorStage = new Stage{
    title = "AST Stage"
    import hexasignal.pico.editor.PicoASTEditor
    scene= new Scene(){ content = new PicoASTEditor(picoVM) }
  }
  picoASTEditorStage.show()
}
