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
import hexasignal.view.PlacingField

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
      field.createRectNode(50.0 + 70.0, 80.0)

      content = new HBox(field) {
        translateX = 10
        translateY = 10
      }
    }
  }


  val viewStage = new Stage(){
    title() = "Hexagon Signal View"

    width = 500
    height = 500
    import hexasignal.model.{ViewModel, ViewModelRenderer, Rect}
    
    val viewModel = new ViewModel()
    viewModel.rect(10,10,10,10, Black, White)
    viewModel.rect(100,100,10,10,White,Black)
    viewModel.line(10,20,100,200,White)

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
}
