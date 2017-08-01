package hexasignal.treeviews

import scalafx.Includes._
import scalafx.scene.shape.{Rectangle, Line}
import scalafx.scene.text.Text
import scalafx.scene.control.{TextField}
import scalafx.scene.input.{KeyEvent, KeyCode}
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.scene.layout.{Pane, HBox, BorderStroke, Region}
import scalafx.scene.input.{MouseEvent, MouseButton, MouseDragEvent}
import scalafx.geometry.{
  Pos,
  Insets,
  Point2D
}
import MouseButton.{Primary, Secondary}
import hexasignal.natsuki.VM
import hexasignal.natsuki.SExpr
import hexasignal.view.DragMoving
import hexasignal.natsuki.editor.CodeEditor
import scala.util.parsing.input.OffsetPosition
import hexasignal.natsuki
import natsuki.Atom


class TreeViews(val vm : VM, val textEditor : CodeEditor ) extends Pane { treeview =>
  import hexasignal.natsuki._

  val unitW = 10
  val unitH = 10
  minWidth = 500
  minHeight = 500

  vm.eventsOnRun :+= {vm:VM =>
    if( children.length > 0){
      children.removeAll(children:_*)
    }

    var currentY : Double = 0
    vm.sexprs.foreach{
      expr =>
      currentY = addSExprNode(0, currentY + 10, SExprNode(expr))
    }
  }

  handleEvent(MouseEvent.MouseClicked) {
    (event:MouseEvent) =>
    if( event.clickCount == 2){
      import scalafx.stage.Stage
      import scalafx.scene.Scene
      import scalafx.scene.layout.HBox
      import scalafx.scene.control.TextField
      import scalafx.scene.control.Button
      val stage = new Stage(){ stage =>
        scene = new Scene(){
          content = new HBox(){
            val textField = new TextField()
            val button = new Button("create!"){
              onAction = { _ =>
                textField.text() match {
                  case text =>
                    import scala.util.control.Exception._
                    val s = catching(classOf[NumberFormatException]) opt text.toInt match {
                      case Some(i) => i 
                      case None => 0
                    }
                }
              }
            }
            children = Seq(textField, button)
          }
        }
      }
      stage.show()
    }
  }
                                       



  def addSExprNode(x:Double, y:Double, node: SExprNode) : Double = {
    node.translateX() = x
    node.translateY() = y
    children.add(node)
    var currentX = x + 100
    var currentY = y + 44
    node.childrenSExprNode().zipWithIndex.foreach(
      {
        case (child, i) =>
          currentY = addSExprNode(currentX, currentY,  child)
      }
    )
    node.lines().foreach(children.add(_))
    node.lines().foreach(_.toBack())
    currentY
  }



  object SExprNode {
    def apply(sexpr: SExpr) : SExprNode =
      sexpr match {
        case s: Atom.Bool => new BoolSExprNode(s)
        case n: Atom.Num => new NumSExprNode(n)
        case str: Atom.Str => new StrSExprNode(str)
        case sym: Atom.Sym => new SymSExprNode(sym)
        case v: Atom.Var => new VarSExprNode(v)
        case l: natsuki.ConsList => new ConsListSExprNode(l)
        case f: natsuki.Fn => new FnSExprNode(f)
        case s: natsuki.SetVar => new SetVarSExprNode(s)
        case i: natsuki.IF => new IFSExprNode(i)
        case l: natsuki.Let => new LetSExprNode(l)
        case a: natsuki.Apply => new ApplySExprNode(a)
      }

    def apply(ref: SExprRef[SExpr]) : SExprNode =
      apply(vm.sexprTable.map(ref))


    def createLine(node1 : Region, node2 : Region) :  Line = {
      val line = Line(0,0,1,1)
      line.startX <== node1.translateX + node1.width / 2
      line.startY <== node1.translateY + node1.height / 2
      line.endX <== node2.translateX + node2.width / 2
      line.endY <== node2.translateY + node2.height / 2
      line
    }

  }
  
  abstract class SExprNode extends HBox {
    spacing = 10
    alignment() = Pos.Center
    padding() = Insets(2,2,2,2)
    style() = "-fx-border-color: black;-fx-background-color: white "
    prefWidth() = 100
    prefHeight() = 33.0

    val sexpr : SExpr
    val childrenSExprNode : ObjectProperty[List[SExprNode]] = ObjectProperty(List())
    val lines : ObjectProperty[List[Line]] = ObjectProperty(List())

    var siblings : List[SExprNode] = List()

    handleEvent(MouseEvent.DragDetected)
    { (event: MouseEvent) =>
      startFullDrag()
    }

    var firstPos : Option[Point2D] = None
    handleEvent(MouseEvent.MousePressed) {
      (event: MouseEvent) =>
      val pos = parent().sceneToLocal(event.sceneX,event.sceneY)
      event.button match {
        case Primary =>
          firstPos = Some(new Point2D(
            translateX() - pos.x,
            translateY() - pos.y
          ))
          event.consume()
        case Secondary =>
        case _ =>
      }
    }

    handleEvent(MouseDragEvent.MouseDragEntered){
      (event: MouseEvent) =>
    }

    handleEvent(MouseDragEvent.MouseDragExited){
      (event: MouseEvent) =>
    }

    handleEvent(MouseDragEvent.MouseDragReleased){
      (event: MouseEvent) =>
    }

    handleEvent(MouseEvent.MouseDragged) {
      (event: MouseEvent) =>
      val pos = parent().sceneToLocal(event.sceneX,event.sceneY)
      translateX() = pos.x + firstPos.get.x
      translateY() = pos.y + firstPos.get.y 
      event.consume()

      val index = siblings.indexOf(this)
      siblings.take(index).filter(_ != null).foreach(node =>
        if( node.translateY() < this.translateY() ){
          vm.swap(node.sexpr, this.sexpr)
        }
      )

      siblings.drop(index).filter(_ != null ).foreach(node =>
        if( node.translateY() > this.translateY() ){
          vm.swap(node.sexpr, this.sexpr)
        }
      )
    }

    handleEvent(MouseEvent.MouseReleased) {
      (event: MouseEvent) =>
    }
  }

  class NumSExprNode(val sexpr: Atom.Num) extends SExprNode {
    val textField = new TextField()
    textField.text() = sexpr match {
      case Atom.Num.I(i, _, _) => i.toString()
      case Atom.Num.D(d, _,  _) => d.toString()
      case Atom.Num.R(n,d,_, _) => s"${n.toString()}/${d.toString()}"
    }

    textField.
      text.
      onChange((_, oldValue, newValue) => {
                 sexpr match {
                   case integer: Atom.Num.I =>
                     import scala.util.control.Exception._
                     catching(classOf[NumberFormatException]) opt newValue.toInt match {
                       case Some(i) =>
                         vm.update(integer.ref, Atom.Num.I(i,integer.pos, Some(integer.ref)))
                       case None =>
                     }
                   case double: Atom.Num.D =>
                     import scala.util.control.Exception._
                     catching(classOf[NumberFormatException]) opt newValue.toDouble match {
                       case Some(d) =>
                         vm.update(double.ref, Atom.Num.D(d, double.pos, Some(double.ref)))
                       case None =>
                     }
                   case rational: Atom.Num.R =>
                     import scala.util.control.Exception
                     try {
                       val Array(n, d) = newValue.split("/")
                       vm.update(rational.ref,
                                 Atom.Num.R(
                                   n.toInt,
                                   d.toInt,
                                   rational.pos,
                                   Some(rational.ref)
                                 )
                       )
                     }catch {
                       case _:Exception =>
                     }
                 }
               })
    val text = new Text(
      sexpr match {
        case _:Atom.Num.I => "int"
        case _:Atom.Num.D => "double"
        case _:Atom.Num.R => "rational"
      }
    )
    children.add(text)
    children.add(textField)
  }
  

  class BoolSExprNode(val sexpr: Atom.Bool) extends SExprNode {
    type S = Atom.Bool
    import scalafx.scene.control.{
      ToggleGroup,
      ToggleButton
    }
    val boolToggleGroup = new ToggleGroup()
    val trueButton = new ToggleButton("true")
    boolToggleGroup.toggles.add(trueButton)
    val falseButton  = new ToggleButton("false")
    boolToggleGroup.toggles.add(falseButton)
    if( sexpr.b ){
      boolToggleGroup.selectToggle(trueButton)
    }else{
      boolToggleGroup.selectToggle(falseButton)
    }

    boolToggleGroup.
      selectedToggle.
      onChange((_, _, newValue) => {
                 if( trueButton.selected() ){
                   vm.update(sexpr.ref,
                             Atom.Bool(true,
                                       sexpr.pos,
                                       Some(sexpr.ref)
                             ))
                 }else{
                   vm.update(sexpr.ref,
                             Atom.Bool(false,
                                       sexpr.pos,
                                       Some(sexpr.ref)
                             ))
                 }
               })

    children.addAll(new Text("bool"), trueButton, falseButton)
  }


  class SymSExprNode(val sexpr: Atom.Sym) extends SExprNode {
    val textField = new TextField()
    textField.text = sexpr.n
    children.addAll(new Text("symbol"), textField)
    textField.text.onChange((_,_,newValue) =>
      vm.update(sexpr.ref,  sexpr.update(newValue))
    )
  }


  class VarSExprNode(val sexpr: Atom.Var) extends SExprNode {
    val textField = new TextField()
    textField.text = sexpr.n
    children.addAll(new Text("var"), textField)
    textField.text.onChange((_,_,newValue) =>
      vm.update(sexpr.ref,  sexpr.update(newValue))
    )
  }

  class StrSExprNode(val sexpr: Atom.Str) extends SExprNode {
    val textField = new TextField()
    textField.text = sexpr.n
    children.addAll(new Text("string"), textField)
    textField.text.onChange((_,_,newValue) =>
      vm.update(sexpr.ref,  sexpr.update(newValue))
    )
  }

 class ConsListSExprNode(val sexpr: natsuki.ConsList) extends SExprNode {
   children.add(new Text("[]"))
   childrenSExprNode() = sexpr.list.map(node => SExprNode(node))
   childrenSExprNode().foreach(
     _.siblings = childrenSExprNode()
   )
   lines() = childrenSExprNode().map(SExprNode.createLine(this, _))
  }

  class FnSExprNode(val sexpr: natsuki.Fn) extends SExprNode {
    val textField =  new TextField()
    textField.text = vm.get(sexpr.name) match {
      case Some(s:Atom.Sym) =>  s.n
      case _ => ""
    }
    children.addAll(new Text("fn"), textField)

    val exprs = sexpr.exprs

    childrenSExprNode() =
      sexpr.args.map(SExprNode(_)) :+
        (new SExprNode { node =>
           val sexpr = null
           children.add(new Text("->"))
           childrenSExprNode() =
             exprs.map(SExprNode(_))
           lines() = childrenSExprNode().map(SExprNode.createLine(node, _))
         })
    lines() = childrenSExprNode().map(SExprNode.createLine(this,_))

    childrenSExprNode().foreach(
      _.siblings = childrenSExprNode()
    )
  }

  class SetVarSExprNode(val sexpr: natsuki.SetVar) extends SExprNode {
    children.add(new Text("set"))
    childrenSExprNode() = List(
      SExprNode(sexpr.name),
      SExprNode(sexpr.expr)
    )
    lines() = childrenSExprNode().map(SExprNode.createLine(this,_))
  }


  class IFSExprNode(val sexpr:natsuki.IF) extends SExprNode {
    children.add(new Text("if"))
    childrenSExprNode() = List(
      SExprNode(sexpr.cond),
      SExprNode(sexpr.thenExpr)
    ) ++ 
      (sexpr.elseExpr match {
         case  Some(t) => List(SExprNode(t))
         case _ => List()
      })
    lines() = childrenSExprNode().map(SExprNode.createLine(this,_))
  }

  class LetSExprNode(val sexpr:natsuki.Let) extends SExprNode {
    children.add(new Text("let"))
    childrenSExprNode() =  sexpr.varMap.toList.map {
      case (v, expr) =>
        val vnode = SExprNode(v)
        vnode.childrenSExprNode() = List(SExprNode(expr))
        vnode.lines() = vnode.childrenSExprNode().map(SExprNode.createLine(vnode,_))
        vnode
    }
    childrenSExprNode() = childrenSExprNode() ++ sexpr.expr.map(SExprNode(_)) 
    lines() = childrenSExprNode().map(SExprNode.createLine(this, _))
  }

  class ApplySExprNode(val sexpr:natsuki.Apply) extends SExprNode {
    children.add(new Text("()"))
    childrenSExprNode() =
      SExprNode(sexpr.func) :: sexpr.args.map(SExprNode(_))
    lines() = childrenSExprNode().map(SExprNode.createLine(this, _))
  }

}
