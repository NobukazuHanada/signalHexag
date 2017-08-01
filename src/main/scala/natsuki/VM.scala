package hexasignal.natsuki


import scala.util.parsing.input.Reader 
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.Parsers

import scalafx.beans.property.{
  ObjectProperty,
  IntegerProperty
}

class VM {
  var sexprs : Seq[SExpr] = Seq()
  var sexprTable : SExprTable = SExprTable()
  val parser = new Parser()
  var positionTable = PositionTable()

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

  def fireEventOnRun : Unit = 
    eventsOnRun.foreach( _.apply(this))


  def addEventsOnChangeSExpr(f:(SExprRef[SExpr], SExpr, SExpr) => Any) =
    eventsOnChangeSExpr = f :: eventsOnChangeSExpr

  def run(text:String) : Unit = {
    var noerror = true
    var next : Reader[Char] = new CharSequenceReader(text)
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



}


