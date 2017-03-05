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
import scalafx.stage.WindowEvent;
import hexasignal.view.PlacingField
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



      field.createRectNode(50.0 + 70.0, 10.0)
      field.createRectNode(50.0 + 80.0, 80.0)
      field.createCodeNode(50.0 + 90.0, 80.0)
      field.createCodeNode(50.0 + 100.0, 80.0)
      field.createCodeNode(50.0 + 160.0, 80.0)

      content = new HBox(field) {
        translateX = 10
        translateY = 10
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
  ,ViewModelEditor, Rect}
  
  val viewModel = new ViewModel()
  val viewStage = new Stage(){
    title() = "Hexagon Signal View"

    width = 500
    height = 500

    val vmr = new ViewModelRenderer(viewModel)
    scene = vmr
    val animationTimer = AnimationTimer((time) =>
      viewModel.updateModel(
        {
          case Rect(x,y,w,h,f,s) =>
            true
          case _ =>
            false
        },
        field.updater
      )
    )
    animationTimer.start()

  }

  viewStage.show()
  viewStage.toFront()

  val viewModelEditorStage = new Stage{
    val vme = new ViewModelEditor(viewModel);
    scene = vme
  }

  viewModelEditorStage.show();

}
