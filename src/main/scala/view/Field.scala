package hexasignal.view

import hexasignal.shape.Hexagon
import hexasignal.shape.Arrow
import hexasignal.model.{ViewData, Rect}
import hexasignal.message._
import scalafx.scene.paint.Color._
import scalafx.scene.layout.Pane
import scalafx.scene.{ Group, Node => FxNode }
import scalafx.Includes._
import scalafx.scene.input.{MouseEvent, MouseButton, MouseDragEvent, DragEvent, TransferMode}
import scala.collection.mutable.Map



class PlacingField extends Pane {
  placingArea =>
  var selectedNode : Option[Node] = None
  val connectors : Map[(Node,Node), Arrow] = Map()

  def mathcers : List[ViewData => Boolean] = 
    (for(((node1,node2), _) <- connectors if node1.to.isEmpty)
    yield (node1.message match {
             case RectMessage(_, _, _, _) =>
               ({
                 case Rect(_, _, _, _, _, _) => true
                 case _ => false
               } : ViewData => Boolean)
             case  _ => { (viewData: ViewData) => false }
           })).toList
  



  def connect(startNode : Node, endNode : Node) {
    val arrow = new Arrow()
    if( !(connectors contains (startNode -> endNode)) ){
      arrow.startX <== startNode.translateX
      arrow.startY <== startNode.translateY
      arrow.endX <== endNode.translateX
      arrow.endY <== endNode.translateY
      children.add(arrow)
      connectors += (startNode -> endNode) -> arrow
      startNode.addToNode(endNode)
    }
  }

  style = "-fx-border-color: white"
  minWidth = 300
  minHeight = 600

  def createRectNode(x : Double, y : Double) {
    import hexasignal.shape.Hexagon.polygon
    val node = new RectNode(){
      translateX = x
      translateY = y
    }

    addNode(node)
  }

  def createCodeNode(x : Double, y : Double) {
    val node = new CodeNode(){
      translateX = x
      translateY = y
    }

    addNode(node)
  }


  def addNode(node: Node) {
    children.add(node)
    node.hexagon.handleEvent(MouseEvent.DragDetected)
    { (event: MouseEvent) =>
      startFullDrag()
    }

    node.hexagon.handleEvent(MouseEvent.MousePressed) {
      (event: MouseEvent) =>
      import scalafx.scene.input.MouseButton.{Primary, Secondary}

      selectedNode = Some(node)
      event.button match {
        case Primary => node.moveStart()
        case Secondary =>
          node.connectStart(placingArea)
        case _ =>
          selectedNode = None
      }
    }

    node.hexagon.handleEvent(MouseDragEvent.MouseDragEntered){
      (event: MouseEvent) =>
      if( selectedNode.nonEmpty && !selectedNode.contains(this) ){
        node.dragEventHover() = true
      }
    }

    node.hexagon.handleEvent(MouseDragEvent.MouseDragExited){
      (event: MouseEvent) =>
      node.dragEventHover() = false
    }

    node.hexagon.handleEvent(MouseDragEvent.MouseDragReleased){
      (event: MouseEvent) =>
      for( n <- selectedNode if n != node ){
        placingArea.connect(n, node)
      }
    }



    node.handleEvent(MouseEvent.MouseDragged) {
      (event: MouseEvent) =>
      val pos = placingArea.sceneToLocal(event.sceneX,event.sceneY)
      if( node.moving ){
        node.moving(pos.x, pos.y)
      }else if( node.connecting ){
        node.connecterMove(pos.x,pos.y)
      }
    }

    node.handleEvent(MouseEvent.MouseReleased) {
      (event: MouseEvent) =>
      node.mouseTransparent = false
      selectedNode = None

      if( node.moving ){
        node.moveEnd()
      }else if( node.connecting ){
        node.connectEnd(placingArea)
      }
    }
  }

  def updater : ViewData => ViewData  = {
    var updaters : List[ViewData => ViewData] =
      for(((node1, node2), _) <- connectors.toList  )
      yield {
        (node1.message, node2.message) match {
          case (RectMessage(x1,y1,w1,h1),RectMessage(x2,y2,w2,h2)) =>
            val diffX = (x2 - x1) / 10
            val diffY = (y2 - y1) / 10
            val diffW = (w2 - w1) / 10
            val diffH = (h2 - h1) / 10

            (viewdata : ViewData) => viewdata match {
              case Rect(x,y,w,h,f,s)  =>
                val newX = (x + diffX) % 500
                val newY = (y + diffY) % 500
                Rect(newX,newY,w+diffW,h+diffH,f,s)
              case x => x
            }
          case (CodeMessage(MRect(mx,my,mw,mh)),
                CodeMessage(Lambda(args, expr)))
              =>
            val checker = (x:Int,y:Int,w:Int,h:Int) => {
              var check = true;
              var varnames : List[(Int, String)] = List();
              for( ((matcher, data), index) <-
                   List((mx,x), (my, y), (mw, w), (mh, h)).zipWithIndex ){
                matcher match {
                  case MInt(i) =>
                    check = false
                  case MVar(name)  =>
                    varnames :+= (index, name)
                  case _=>
                }
              }
              if( check ){ varnames }else{ List() }
            }
            (viewData: ViewData) => viewData match {
              case rect @ Rect(x,y,w,h,f,s) =>
                var check = true;
                var varnames : List[(Int, String)] = List();
                for( ((matcher, data), index) <-
                     List((mx,x), (my, y), (mw, w), (mh, h)).zipWithIndex ){
                  matcher match {
                    case MInt(i) =>
                      check = false
                    case MVar(name)  =>
                      varnames :+= (index, name)
                    case _=>
                  }
                }
                if( check ){
                  expr match {
                    case AAdd(AVar(name), AInt(i)) =>
                      var newX = x
                      var newY = y
                      var newW = w
                      var newH = h
                      for((index,name) <- varnames ){
                        if( name == name ){
                          index match {
                            case 0 =>
                              newX += i
                            case 1 =>
                              newY += i
                            case 2 =>
                              newW += i
                            case 3 =>
                              newH += i
                            case _ => 
                          }
                        }
                      }
                      Rect(newX, newY, newW, newH, f, s)
                    case _ => rect
                  }
                }else {
                  rect
                }
              case x => x
            }
          case _ => (x:ViewData) => x
        }
      }
    updaters.foldLeft((x:ViewData)=>x)((oldfunc, addfunc) => (x) => addfunc(oldfunc(x)))
  }
}

