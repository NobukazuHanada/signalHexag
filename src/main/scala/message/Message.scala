package hexasignal.message

import scala.util.parsing.combinator._

sealed abstract class Message
case class RectMessage(x : Double, y : Double, width : Double, height : Double) extends Message
case class CodeMessage(code: Code) extends Message

sealed abstract class Code
case class MRect(x:Matcher, y:Matcher, w:Matcher, h:Matcher) extends Code
case class Lambda(arg:AVar, expr: AlterExpr) extends Code
case object NoCode extends Code

abstract class Matcher
case class MVar(name:String) extends Matcher
case class MInt(name:Int) extends Matcher
case object MUnderScore extends Matcher
case object MNo extends Matcher

abstract class AlterExpr
case class AVar(name:String) extends AlterExpr
case class AInt(n:Int) extends AlterExpr
case class AAdd(left:AlterExpr, right:AlterExpr) extends AlterExpr


object CodeCompiler extends RegexParsers {
  def mvar : Parser[MVar] = """[A-Z]""".r ^^ { MVar(_) }
  def mvalue : Parser[MInt] = """-?\d+""".r ^^ { x =>MInt(x.toInt) }
  def munderScore : Parser[Matcher] = "_" ^^ { _ => MUnderScore }

  def matcher : Parser[Matcher] = mvar | mvalue | munderScore

  def avar : Parser[AVar] = """[A-Z]""".r ^^ { AVar(_) }
  def aint : Parser[AInt] = """-?\d+""".r ^^ { x => AInt(x.toInt) }
  def aadd : Parser[AAdd] = (avar | aint) ~ "+" ~ (avar | aint) ^^ { case left~"+"~right => AAdd(left, right)  }

  def lambda : Parser[Lambda] = avar ~ "->" ~ aadd ^^ {
    case v~"->"~expr => Lambda(v,expr)
  }

  def rect : Parser[MRect] = "rect"~>repsep(matcher, "") ^^  {
    case x :: Nil => MRect(x, MNo, MNo, MNo)
    case x :: y :: Nil => MRect(x, y, MNo, MNo)
    case x :: y :: w :: Nil => MRect(x,y,w,MNo)
    case x :: y :: w :: h :: _ => MRect(x,y,w,h)
  }

  def code : Parser[Code] = lambda | rect

  def read(text:String) : Code = {
    parseAll(code, text) match {
      case Success(result, _) => result
      case _ => NoCode
    }
  }

  
}
