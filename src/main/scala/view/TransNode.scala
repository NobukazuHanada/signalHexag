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
  ObjectProperty,
  StringProperty
}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.parsing.input.CharSequenceReader
import hexasignal.natsuki.editor.CodeEditor
import hexasignal.natsuki.{
  VM,
  FilterParser,
  SExpr,
  SExprRef,
  Atom,
  PositionTable,
  SExprTable,
  MetaVar,
  Position,
  PositionRef,
  Range
}
import hexasignal.treeviews.TreeViews

class TransNode(val vm: VM) extends HexNode("trans") {


  val filterParser = new FilterParser()
  val title = new Label("trans inspector")
  title.textFill = Color.White
  val space = new Label("")
  val label = new Label("trans code")
  label.textFill = Color.White
  val textField = new TextArea()
  val vbox = new VBox(10, title, space, label, textField)
  vbox.padding = scalafx.geometry.Insets(10,10,10,10)

  val transText : StringProperty = StringProperty("")
  val transData : ObjectProperty[List[SExpr]] = ObjectProperty(List())

  override def receive(m:Message) : Unit = {
    m match {
      case Message.MCode(code, meta) =>
        if( transData().isEmpty ){

        }else{
          trans(code, transData()(0), meta)
        }
      case _ =>
    }
  }


  transText <== textField.text
  transText.onChange { (_,_,newValue) =>
    filterParser.positionTable = PositionTable()
    filterParser.sexprTable = SExprTable()
    filterParser.parse(filterParser.sexprs,
                       new CharSequenceReader(newValue)) match {
      case filterParser.Success(result, next) =>
        transData() = result
      case filterParser.Error(msg, next) =>
      case filterParser.Failure(msg, next) =>
    }
  }

  def transCodeMove(inputCode:SExpr, positionTable: PositionTable) : Unit = {
    import Atom.Num._
    val startPos1 = vm.positionTable.map(inputCode.pos.start).pos
    var newPositionTable = vm.positionTable
    for( (ref, Position(pos, _)) <- positionTable.map ){
      newPositionTable = newPositionTable + Position(
        startPos1 + pos, ref
      )
    }
    vm.positionTable = newPositionTable
  }

  def trans(inputCode: SExpr, transCode: SExpr, meta : Map[String, SExpr]
  ) : Unit = {
    import Atom.Num._
    import Atom._
    import hexasignal.natsuki.{
      Apply, Let, SetVar, Fn, ConsList
    }

    import hexasignal.natsuki.UnParser

    filterParser.positionTable = PositionTable()
    filterParser.sexprTable = SExprTable()
    filterParser.parse(filterParser.sexprs,
                       new CharSequenceReader(textField.text())) match {
      case filterParser.Success(result, next) =>
        transData() = result
      case filterParser.Error(msg, next) =>
      case filterParser.Failure(msg, next) =>
    }

    var successed = true
    var metaSEprTable: Map[SExprRef[SExpr], SExpr]  = Map()
    for((ref, sexpr) <- filterParser.sexprTable.map ) {
      sexpr match {
        case MetaVar(s,_,_) =>
          println(s)
          val trcExpr = TransNode.parse(s)
          trcExpr match {
            case Some(trcExpr1) =>
              try {
                val s = trcExpr1.run(meta, sexpr.pos)
                metaSEprTable = metaSEprTable + (ref -> s)
              }catch {
                case e: Exception => successed = false
              }
            case _ =>
              successed = false
          }
        case _ => 
      }
    }

    if( successed ) {
      val newExprTable = new SExprTable(
        filterParser.sexprTable.map ++ metaSEprTable
      )
      filterParser.sexprTable = newExprTable

      val text = UnParser.unParse(transCode, newExprTable)
      val inputText = UnParser.unParse(inputCode, vm.sexprTable)
      val natsukiParser = new FilterParser()
      val result = natsukiParser.parse(natsukiParser.sexpr, text)
      val newExpr = result.get
      val newExpr2 = newExpr.updateRef(inputCode.ref)
      vm.move(inputCode.pos.end,text.length() - inputText.length() )
      transCodeMove(inputCode, natsukiParser.positionTable)
      vm.replace(inputCode, text)
      vm.sexprTable =
        new SExprTable(vm.sexprTable.map ++ natsukiParser.sexprTable.map)+ newExpr2
      send(Message.MCode(newExpr2, Map()))
    }
  }
}

import scala.util.parsing.combinator.RegexParsers
object TransNode extends RegexParsers {
  import Atom.Num._
  sealed abstract class TransCode {
    def run(map:Map[String, SExpr], pos: Range) : SExpr
  }
  case class TInt(i:Int) extends TransCode {
    def run(map:Map[String,SExpr], pos : Range): SExpr = I(i,pos)
  }
  case class TDouble(d:Double) extends TransCode {
    def run(map:Map[String,SExpr], pos: Range): SExpr = D(d,pos)
  }
  case class TVar(name:String) extends TransCode {
    def run(map:Map[String,SExpr], pos: Range): SExpr = map(name)
  }
  case class TAdd(left:TransCode, right:TransCode) extends TransCode{
    def run(map:Map[String,SExpr], pos : Range): SExpr = {
      (left.run(map,pos), right.run(map,pos)) match {
        case (I(i1,_,_), I(i2,_,_)) => I(i1 + i2, pos)
        case (D(d1,_,_), D(d2,_,_)) => D(d1 + d2, pos)
        case (I(i,_,_), D(d,_,_)) => D(i + d, pos)
        case (D(d,_,_), I(i,_,_)) => D(i + d, pos)
      }
    }
  }
  case class TSub(left:TransCode, right:TransCode) extends TransCode {
    def run(map:Map[String,SExpr], pos : Range): SExpr = {
      import Atom.Num._
        (left.run(map,pos), right.run(map,pos)) match {
        case (I(i1,_,_), I(i2,_,_)) => I(i1 - i2, pos)
        case (D(d1,_,_), D(d2,_,_)) => D(d1 - d2, pos)
        case (I(i,_,_), D(d,_,_)) => D(i - d, pos)
        case (D(d,_,_), I(i,_,_)) => D(d - i, pos)
      }
    }
  }

  

  def tint: Parser[TInt] = """-?\d+""".r ^^ (_.toInt) ^^ (TInt(_))
  def tdouble: Parser[TDouble] =
    """-?\d+\.(\d)*""".r ^^ (_.toDouble) ^^ (TDouble(_))

  def tvar: Parser[TVar] = """[a-z_][[a-zA-Z_]]""".r ^^ { TVar(_) }


  def tadd : Parser[TAdd] =
    const  ~ "+" ~ const ^^ {
      case transCode1~_~transCode2 =>
        TAdd(transCode1, transCode2)
    }

  def tsub : Parser[TSub] =
    const  ~ "-" ~ const ^^ {
      case transCode1~_~transCode2 =>
        TSub(transCode1, transCode2)
    }


  

  def const : Parser[TransCode] = tint | tdouble | tvar
  def transCode : Parser[TransCode] =
    tadd | tsub  | const

  def parse(s:String): Option[TransCode] =
    parse(transCode, s) match {
      case Success(result, next) =>
        println(result)
        Some(result)
      case _ => None
    }
}
