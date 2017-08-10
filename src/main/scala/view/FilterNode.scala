package hexasignal.view

import akka.actor.{Actor, ActorRef, Props}
import hexasignal.IDGenerator
import hexasignal.Id
import scalafx.Includes._
import scalafx.scene.text.{Text, TextAlignment}
import scalafx.scene.paint.Color
import scalafx.scene.input.MouseEvent
import scalafx.scene.Scene
import scalafx.stage.Stage
import scalafx.scene.layout.{
  HBox,
  VBox
}
import scalafx.scene.control.{
  TextField,
  TextArea,
  Label
}
import scalafx.beans.property.{
  ObjectProperty
}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.parsing.input.CharSequenceReader
import hexasignal.natsuki.editor.CodeEditor
import hexasignal.natsuki.{VM,
  FilterParser,
  SExpr,
  SExprRef
}
import hexasignal.treeviews.TreeViews

class FilterNode(val vm: VM) extends HexNode("filter") {
  val filterParser = new FilterParser()
  val filterExpr : ObjectProperty[List[SExpr]]  = ObjectProperty(List())
  val title = new Label("filter inspector")
  title.textFill = Color.White
  val space = new Label("")
  val label = new Label("pattern")
  label.textFill = Color.White
  val textField = new TextArea()
  val label2 = new Label("guard")
  label.textFill = Color.White
  val textField2 = new TextArea()
  val vbox = new VBox(10, title, space, label, textField, label2, textField2)
  vbox.padding = scalafx.geometry.Insets(10,10,10,10)

  textField.text.onChange {  (_,_,newText) =>
    filterParser.parse(filterParser.sexprs, new CharSequenceReader(newText)) match {
      case filterParser.Success(item, in1) =>
        filterExpr() = item
      case filterParser.Error(_,_) =>
      case filterParser.Failure(_,_) =>
    }
  }

  override def receive(m:Message) : Unit = {
    m match {
      case Message.MCode(code,map) =>
        if( filterExpr().isEmpty){
        
        }else {
          filtering(filterExpr()(0), code, map) match {
            case Some(newMap) =>
              send(Message.MCode(code, newMap))
            case _ =>
          }
        }
      case _ =>
    }
  }


  def filtering(filterExpr : SExpr, inputCode: SExpr, map: Map[String, SExpr] = Map())
      : Option[Map[String,SExpr]] = {
    val filterSExprTable = filterParser.sexprTable
    val inputSExprTable = vm.sexprTable
    import hexasignal.natsuki._
    import hexasignal.natsuki.Atom._
    import hexasignal.natsuki.Atom.Num._

      (filterExpr, inputCode)  match {
        case (MetaVar(name,_,_), code) =>
          Some(map + (name -> code)) 
        case (Sym("_",_,_), code) =>
          Some(map)
        case (Sym(n1,_,_), Sym(n2,_,_)) if n1 == n2 =>
          Some(map)
        case (Var(n1,_,_), Var(n2,_,_)) if n1 == n2 =>
          Some(map)
        case (Str(n1,_,_), Str(n2,_,_)) if n1 == n2 =>
          Some(map)
        case (I(n1,_,_), I(n2,_,_)) if n1 == n2 =>
          Some(map)
        case (D(n1,_,_), D(n2,_,_)) if n1 == n2 =>
          Some(map)
        case (R(n1,d1,_,_), R(n2,d2,_,_)) if n1 == n2 && d1 == d2 =>
          Some(map)
        case (Bool(n1,_,_), Bool(n2,_,_)) if n1 == n2 =>
          Some(map)
        case (ConsList(list1,_,_), ConsList(list2,_,_)) =>
          filterings(list1, list2, map)
        case (Apply(fun1,args1,_,_),
              Apply(fun2,args2,_,_)) =>
          filtering(filterSExprTable.map(fun1),
                    inputSExprTable.map(fun2)).flatMap( map =>
            filterings(args1, args2, map)
          )
        case (Fn(name1, args1, exprs1,_,_),
              Fn(name2, args2, exprs2,_,_)) =>
          val name1Expr = filterSExprTable.map(name1)
          val name2Expr = inputSExprTable.map(name2)
          for {
            map1 <- filtering(name1Expr,name2Expr,map)
            map2 <- filterings(args1, args2, map)
            map3 <- filterings(exprs1, exprs2, map)
          } yield map1 ++ map2 ++ map3
        case (SetVar(v1, expr1,_,_), SetVar(v2, expr2,_,_)) =>
          val v1expr = filterSExprTable.map(v1)
          val v2expr = inputSExprTable.map(v2)
          val e1expr = filterSExprTable.map(expr1)
          val e2expr = inputSExprTable.map(expr2)
          for {
            map1 <- filtering(v1expr, v2expr, map)
            map2 <- filtering(e1expr, e2expr, map)
          } yield map1 ++ map2

        case (Let(varMap1, expr1,_,_), Let(varMap2, expr2, _,_ )) =>
          if( varMap1.size == varMap2.size ){
            val firstObj : (List[SExprRef[SExpr]], List[SExprRef[SExpr]]) =
              (List(), List())
            val (keys1, exprs1) = varMap1.foldLeft(firstObj) {
              (acc, keyExpr) =>
              (keyExpr._1 :: acc._1, keyExpr._2 :: acc._2)
            }
            val (keys2, exprs2) = varMap2.foldLeft(firstObj) {
              (acc, keyExpr) =>
              (keyExpr._1 :: acc._1, keyExpr._2 :: acc._2)
            }
            for{
              map1 <- filterings(keys1, keys2, map)
              map2 <- filterings(exprs1, exprs2, map)
              map3 <- filterings(expr1, expr2, map)
            } yield map1 ++ map2 ++ map3
          }else{
            None
          }

        case (IF(cond1, thenExpr1, elseExpr1opt,_,_),
              IF(cond2, thenExpr2, elseExpr2opt,_,_)) =>
          (elseExpr1opt, elseExpr2opt) match {
            case (None, None) =>
              for {
                map1 <- filtering(cond1, cond2, map)
                map2 <- filtering(thenExpr1, thenExpr2, map)
              } yield map1 ++ map2
            case (Some(elseExpr1), Some(elseExpr2)) =>
              for {
                map1 <- filtering(cond1, cond2, map)
                map2 <- filtering(thenExpr1, thenExpr2, map)
                map3 <- filtering(elseExpr1, elseExpr2, map)
              } yield map1 ++ map2 ++ map3
          }

        case _=> None
    }
  }

  def filtering(filterExpr: SExprRef[SExpr], inputExpr: SExprRef[SExpr], map:
                    Map[String, SExpr]
  ) : Option[Map[String, SExpr]] = {
    filtering(
      filterParser.sexprTable.map(filterExpr),
      vm.sexprTable.map(inputExpr),
      map
    )
  }

  def filterings(filterExprs: List[SExprRef[SExpr]],
                 inputExprs: List[SExprRef[SExpr]],
                 map: Map[String, SExpr]) : Option[Map[String, SExpr]] = 
    if( filterExprs.length == inputExprs.length ) {
      val filterSExprTable = filterParser.sexprTable
      val inputSExprTable = vm.sexprTable
        filterExprs.zip(inputExprs).foldRight(Some(map):Option[Map[String, SExpr]]) {
          (expr12, result) =>
          result match {
            case None => None
            case Some(map) =>
              filtering(filterSExprTable.map(expr12._1),
                        inputSExprTable.map(expr12._2),
                        map)
          }
        }
      } else {
        None
    }

}
