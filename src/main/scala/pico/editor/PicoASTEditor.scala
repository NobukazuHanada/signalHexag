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
    import scala.math._
    val centerX = width() / 2
    val centerY = height() / 2
    var numOfAllNodes = 0
    var radius = width() / 2 - 10
    var maxLevel = 0
    var position = 0
    var minAngle =  Pi / 90
    var maxAngle =  Pi / 10
    var counterOfCreatedAngle = 0
    var currentAngle : Double = 0
    import scala.collection.mutable.Map
    val numOfNodesInLevel : Map[Int, Int] = Map()

    def apply(ast: PicoExpr, level : Int, _parent:TreeView): Node = {
      numOfAllNodes += 1
      numOfNodesInLevel.get(level) match {
        case Some(_) => numOfNodesInLevel(level) += 1
        case None =>  numOfNodesInLevel(level) = 1
      }
      if( maxLevel < level ) maxLevel = level
      val node = new Node(ast, level, position){
        val parent = _parent
      }
      if( node.children.isEmpty ){
        position += 1
      }
      node
    }

    def reset : Unit = {
      numOfAllNodes = 0
      radius = width() / 2 - 10
      maxLevel = 0
      position = 0
      minAngle =  Pi / 90
      maxAngle =  Pi / 10
      counterOfCreatedAngle = 0
      currentAngle = 0
      numOfNodesInLevel.clear()
    }

  }

  sealed abstract class TreeView
  object Root extends TreeView
  abstract class Node private(val ast: PicoExpr, val level : Int, val pos: Int) extends TreeView{
    val parent : TreeView

    import Node._
    import scala.math._


    
    def nodeRadius = (radius / maxLevel) * level
    def x = centerX + nodeRadius * sin(angle)
    def y = centerY + nodeRadius * cos(angle) 
    val w = 5.0
    val h = 5.0
    val text : String =
      ast match {
        case PicoInt(i) => i.toString()
        case PicoFloat(f) => f.toString()
        case PicoString(s) => '"' + s + '"'
        case PicoSymbol(s) => s
        case PicoTrue => "true"
        case PicoFalse => "false"
        case PicoList(_ @ _*) => "list"
        case PicoLambda(_, _ @ _*) => "lambda"
        case PicoDefine(_,_ @ _*) => "defar"
        case PicoDefineLambda(_,_, _ @ _*) => "defun"
        case PicoIf(_,_,_) => "if"
        case PicoLet(_,_ @_*) => "let"
        case PicoApply(_,_ @_*) => "apply"
        case a =>
          println(a)
          a.toString()
      }


   
    def angle : Double = {
      if( children.isEmpty ){
        if( numOfNodesInLevel(1) < 3  ){ 
          pos * Pi / (position * 2)
        }else if( numOfNodesInLevel(1) < 5 ) {
          pos * Pi / position
        }else{
          2 * pos * Pi / position
        }
      }else{
        val childrenAngles = children.map(_.angle)
        val intervalAngleOfChildren = childrenAngles.max - childrenAngles.min
        childrenAngles.min + intervalAngleOfChildren / 2
      }
    }

    def draw(gc: GraphicsContext) : Unit = {
      var nodeRadius = (radius / maxLevel) * level
      gc.stroke = Color(1,1,1,0.5)
      gc.strokeRect(x - w/2,y - h/2,w,h)
      parent match {
        case Root => gc.strokeLine(x , y, centerX, centerY)
        case node:Node => gc.strokeLine(x, y, node.x, node.y)
      }
      gc.stroke = Color(1,1,1,1)
      gc.strokeText(text,x-w/2, y-h/2)
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
        case picoLambda@PicoLambda(PicoArgs(args @_*), exprs @ _*) =>
          args.map(Node(_, level + 1, this)) ++ exprs.map(Node(_, level + 1, this))
        case picoDefine@PicoDefine(name:String, exprs @ _*) =>
         Node(PicoSymbol(name), level + 1, this) +: exprs.map(Node(_, level + 1, this))
        case picoDefineLambda@PicoDefineLambda(name:String, PicoArgs(args @ _*), exprs @ _*) =>
         Node(PicoSymbol(name),level+1, this) +: (args.map(Node(_, level+1, this)) ++ exprs.map(Node(_, level + 1, this)))
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
    Node.reset
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
    gc.strokeText("ROOT", width()/2, height()/2)
    for(node <- nodes){
      node.draw(gc)
    }
    println(Node.numOfNodesInLevel)
  }


}
