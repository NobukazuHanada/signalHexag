package hexasignal.natsuki


import scala.util.parsing.input.Reader 
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.Parsers

import scalafx.beans.property.{
  ObjectProperty,
  IntegerProperty,
  StringProperty
}

import scalafx.scene.canvas.Canvas
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.Scene
import scalafx.stage.Stage
import scalafx.scene.paint.Color

class VM {
  val text : StringProperty = StringProperty("")
  var sexprs : Seq[SExpr] = Seq()
  var sexprTable : SExprTable = SExprTable()
  val parser = new Parser()
  var positionTable = PositionTable()

  def getText[S <: SExpr](sexpr: SExpr) : String = {
    val range = sexpr.pos
    val startPos = positionTable.map(range.start).pos
    val endPos = positionTable.map(range.end).pos
    text().substring(startPos, endPos)
  }

  def get[S <: SExpr](ref: SExprRef[S]) : Option[S] =
    (sexprTable.map get ref).map(_.asInstanceOf[S])

  def update[S <: SExpr](ref: SExprRef[S], newValue:S) : Unit = {
    val oldValue = sexprTable.map(ref)
    sexprTable = sexprTable.update(ref, newValue)
    eventsOnChangeSExpr.foreach(_.apply(ref, oldValue, newValue))
  }

  def swap[S <: SExpr, T <: SExpr](s:S,t:T) : Unit = {
  }

  var eventsOnRun : List[VM => Any] = List()
  var eventsOnChangeSExpr : List[(SExprRef[SExpr], SExpr, SExpr) => Any] = List()
  var eventsOnSwap : List[(SExpr, SExpr) => Any] = List()

  def fireEventOnRun : Unit = 
    eventsOnRun.foreach( _.apply(this))

  def addEventsOnChangeSExpr(f:(SExprRef[SExpr], SExpr, SExpr) => Any) =
    eventsOnChangeSExpr = f :: eventsOnChangeSExpr

  def addEventOnSwap(f:(SExpr, SExpr) => Any) : Unit =
    eventsOnSwap = f :: eventsOnSwap

  def reRun(text:String, sexpr: SExpr) : Unit = {


  }

  def treeUpdate() : Unit = {
    // fireEventOnRun
  }

  def delelte(s:SExpr) :Unit = {
    val startPos = positionTable.map(s.pos.start).pos
    val endPos = positionTable.map(s.pos.end).pos

    val text1 = text().substring(0, startPos)
    val text2 = text().substring(endPos)
    text() = text1 + text2
  }

  def move(pos : PositionRef, addPos : Int) : Unit = {
    val posInt = positionTable.map(pos).pos
    val positionTableMap = positionTable.map.map { case (ref, currentPos) =>
      if( currentPos.pos > posInt ) {
        (ref, Position(currentPos.pos + addPos, ref))
      }else{
        (ref, currentPos)
      }
    }
    positionTable = PositionTable(positionTableMap)
  }

  def replace(s:SExpr, t:String) : Unit = {
    val startPos = positionTable.map(s.pos.start).pos
    val endPos = positionTable.map(s.pos.end).pos

    val text1 = text().substring(0, startPos)
    val text2 = text().substring(endPos)
    text() = text1 + t + text2


  }


  def run(text:String, pos : Int = 0, optionEnd: Option[Int] = None ) : Unit = {
    val subString = optionEnd match {
      case None => text
      case Some(end) => text.substring(0, end)
    }

    var noerror = true
    var next : Reader[Char] = new CharSequenceReader(subString, pos)
    sexprs = Seq()
    parser.parse(parser.sexprs, next) match {
      case parser.Success(sexprs1, next2) =>
        next = next2
        sexprs = sexprs1
      case parser.Error(msg, next2) =>
        noerror = false
        next = next2
      case parser.Failure(msg, next2) =>
        noerror = false
        next = next2
    }
    sexprTable = parser.sexprTable
    positionTable = parser.positionTable
    fireEventOnRun
  }

  val canvas = new Canvas(500, 500)
  val stage = new Stage() {
    width = 500
    height = 500
    scene = new Scene(){
      content = canvas
    }
  }

  var evalSet : Set[SExpr] = Set()

  val g = canvas.graphicsContext2D
  g.setStroke(Color.Black)
  g.setFill(Color.White)

  def eval(s:SExpr) : Unit =
    if( !(evalSet contains s) ) {
      import Atom._
      import Num._
      evalSet += s
      s match {
        case Apply(name, exprs, _, _) =>
          sexprTable.map(name) match {
            case Sym("rect",_,_) =>
              exprs.map(sexprTable.map(_)) match {
                case List(I(i1,_,_),
                          I(i2,_,_),
                          I(i3,_,_),
                          I(i4,_,_),
                          rest @_* ) =>
                  g.strokeRect(i1, i2, i3, i4)
                  g.rect(i1, i2, i3, i4)
                  stage.show()
                case _ =>
              }
            case Sym("stroke",_,_) =>
              exprs.map(sexprTable.map(_)) match {
                case List(I(i1,_,_),
                          I(i2,_,_),
                          I(i3,_,_),
                          I(i4,_,_),
                          rest @_* ) =>
                  g.setStroke(Color.rgb(i1, i2, i3))

                case _ =>
              }
            case Sym("fill",_,_) =>
              exprs.map(sexprTable.map(_)) match {
                case List(I(i1,_,_),
                          I(i2,_,_),
                          I(i3,_,_),
                          I(i4,_,_),
                          rest @_* ) =>
                  g.setFill(Color.rgb(i1, i2, i3))

                case _ =>
              }
            case _ =>
          }
        case _ =>
      }

    }
}


