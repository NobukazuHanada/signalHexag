package com.nobkz.hexagonSingal;

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
import com.nobkz.hexagonSingal.view.PlacingField

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



object Main extends JFXApp {
  val viewStage = new Stage(){
    title() = "Hexagon Signal View"
    width = 500
    height = 500
    import com.nobkz.hexagonSingal.model.{ViewModel, ViewModelRenderer, Rect}
   
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
        {
          case Rect(x,y,w,h,f,s) =>
            Rect(x+1,y,w,h,f,s)
          case x => x
        }
      )
    )
    animationTimer.start()

  }

  viewStage.show()

  stage = new JFXApp.PrimaryStage {
    title.value = "Hexagon Signal Editor"
    width = 930
    height = 730

    scene = new Scene {
      fill = Black


      val group1 = new PlacingField()
      for( i <- 1 to 3 )
        group1.createNode(i * 50.0 + 40.0, i * 10.0)


      val group2 = new PlacingField()
      for( i <- 1 to 3 )
        group2.createNode(i * 50.0 + 40.0, i * 10.0)

      content = new HBox(group1, group2) {
        translateX = 10
        translateY = 10
      }
    }
  }
}
