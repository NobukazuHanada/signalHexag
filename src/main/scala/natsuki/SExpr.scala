package hexasignal.natsuki

import scala.util.parsing.input.{
  Position,
  NoPosition,
  OffsetPosition,
  Positional
}

trait Range extends Positional{
  var endPos : Position = NoPosition
  def setEndPos(newendpos:Position) : this.type = {
    if(endPos eq NoPosition) endPos = newendpos
    this
  }

  def getInputText(sourceText: String) : Option[String] =
    (pos, endPos) match {
      case (startPos:OffsetPosition, endPos:OffsetPosition) =>
        Some(sourceText.substring(startPos.offset, endPos.offset))
      case _ => None
    }
}

sealed abstract class SExpr extends Range

abstract class Cons extends SExpr
case class DataList(list:List[SExpr]) extends Cons
case class EvalList(list:List[SExpr]) extends Cons

sealed abstract class Atom extends SExpr{
}

object Atom {
  case class Sym(name: String) extends Atom
  case class Var(name: String)  extends Atom
  case class Str(content:String) extends Atom
  abstract class Num extends Atom
  object Num {
    case class I(i:Int) extends Num{
      override def toString : String = i.toString()
    }
    case class D(d:Double) extends Num{
      override def toString : String = d.toString()
    }
    case class R(n:Int, d:Int) extends Num{
      override def toString : String = s"$n/$d"
    }
  }
  abstract class Bool extends Atom
  object Bool{
    case object True extends Bool
    case object False extends Bool
  }
}
