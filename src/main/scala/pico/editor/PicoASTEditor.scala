package hexasignal.pico.editor

import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.canvas.GraphicsContext
import hexasignal.pico.PicoVM
import hexasignal.pico.Pico._


class PicoASTEditor(val vm: PicoVM) extends Canvas {
  width = 600
  height = 600

  object Node{
    val centerX = width() / 2
    val centerY = height() / 2
    var numOfAllNodes = 0
    var radius = width() / 2 - 10
    var maxLevel = 0
    import scala.collection.mutable.Map
    val numOfNodesInLevel : Map[Int, Int] = Map()

    def apply(ast: PicoExpr, level : Int, _parent:TreeView): Node = {
      numOfAllNodes += 1
      numOfNodesInLevel.get(level) match {
        case Some(_) => numOfNodesInLevel(level) += 1
        case None =>  numOfNodesInLevel(level) = 1
      }
      if( maxLevel < level ) maxLevel = level
      new Node(ast, level, numOfNodesInLevel(level)){
        val parent = _parent
      }
    }


  }

  sealed abstract class TreeView
  object Root extends TreeView
  abstract class Node private(val ast: PicoExpr, val level : Int, val pos: Int) extends TreeView{
    val parent : TreeView

    import Node._
    import scala.math._

    def nodeRadius = (radius / maxLevel) * level
    def x = centerX + nodeRadius * sin(2.0 * Pi * pos/numOfNodesInLevel(level))
    def y = centerY + nodeRadius * cos(2.0 * Pi * pos/numOfNodesInLevel(level))
    val w = 5.0
    val h = 5.0

    def draw(gc: GraphicsContext) : Unit = {
      var nodeRadius = (radius / maxLevel) * level
      gc.strokeRect(x - w/2,y - h/2,w,h)
      gc.strokeText(ast.toString(),x-w/2, y-h/2)
      parent match {
        case Root => gc.strokeLine(x , y, centerX, centerY)
        case node:Node => gc.strokeLine(x, y, node.x, node.y)
      }
      children.foreach(_.draw(gc))
    }

    val children : Seq[Node] =
      ast match {
        case atom@(PicoInt(_)
                     | PicoFloat(_)
                     | PicoString(_)
                     | PicoSymbol(_)
                     | PicoTrue
                     | PicoFalse) =>
          List()
        case picoList@PicoList(exprs @_*) =>
          exprs.map(expr=>Node(expr, level + 1, this))
        case picoLambda@PicoLambda(PicoArgs(args @_*), expr) =>
          args.map(Node(_, level + 1, this)) :+ Node(expr, level + 1, this)
        case picoDefine@PicoDefine(name:String, expr) =>
         List(Node(PicoSymbol(name), level + 1, this), Node(expr, level + 1, this))
        case picoDefineLambda@PicoDefineLambda(name:String, PicoArgs(args @ _*), expr) =>
         Node(PicoSymbol(name),level+1, this) +: args.map(Node(_, level+1, this)) :+ Node(expr, level+1, this)
        case picoIf@PicoIf(cond, thn, None) =>
          List(Node(cond,level+1, this), Node(thn,level+1, this))
        case picoIf@PicoIf(cond,thn,Some(els)) =>
          List(Node(cond,level+1, this), Node(thn,level+1, this), Node(els,level+1, this))
        case picoLet@PicoLet(bindings, exprs @_*) =>
          import scala.collection.mutable.ListBuffer
          var childrens : ListBuffer[Node] = ListBuffer()
          val BindingMap(map) = bindings
          for((bind,value) <- map){
            childrens += Node(bind, level+1, this)
            childrens += Node(value, level+1, this)
          }
          for(expr <- exprs) childrens += Node(expr, level+1, this)
          childrens.toList
        case picoApply@PicoApply(func, args @_*)  =>
          Node(func,level+1, this) +: args.map(Node(_, level+1, this))
      }
  }


  vm.addFireEnvEvent {
    env =>
    draw
    true
  }

  def draw : Unit = {
    val ast = vm.ast
    val nodes = ast match {
      case PicoSentence(exprs @ _*) =>
        exprs.map(Node(_, 1, Root))
    }
    val gc = this.getGraphicsContext2D
    gc.setFill(Color.Black)
    gc.fillRect(0, 0, width(), height())
    gc.setStroke(Color.White)
    gc.strokeRect(width()/2 - 5, height()/2 - 5, 10, 10)
    for(node <- nodes){
      node.draw(gc)
    }
    println(Node.numOfNodesInLevel)
  }


}
