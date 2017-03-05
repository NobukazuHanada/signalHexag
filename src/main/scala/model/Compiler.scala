package hexasignal.model

import scala.util.parsing.combinator._
import scala.util.parsing.input._


object Compile extends RegexParsers{


  sealed class SExpr
  case class SList(l:List[SExpr]) extends SExpr
  case class Number(n:Double) extends SExpr
  case class Symbol(s:String) extends SExpr
  case object Rect extends SExpr
  case object Line extends SExpr
  case object X extends SExpr
  case object Y extends SExpr
  case object width extends SExpr
  case object height extends SExpr


  def rect : Parser[SExpr] = "rect" ^^ { _ => Rect }
  def line : Parser[SExpr] = "line" ^^ { _ => Line }

  def number : Parser[Number] = """-?\d+(\.\d*)?""".r ^^ { x => Number(x.toDouble) } 
  def symbol : Parser[Symbol] = """[a-zA-Z_-]""".r ^^ { x => Symbol(x.toString) }

  def atom : Parser[SExpr] = rect | line | number | symbol

  def sexpr : Parser[SExpr] =
    (atom 
       | "("~>repsep(sexpr, "")<~")" ^^ { SList(_) }
    )
  

  def read(text:String, acc : List[SExpr] = List()) : (List[SExpr], String) = {
    parse(sexpr, text) match {
      case Success(result, rest) =>
        read(rest, result :: acc)
      case _ =>
        (acc.reverse, text)
    }
  }

  def read(text: Reader[Char], acc : List[SExpr]) : (List[SExpr], String) = {
    parse(sexpr, text) match {
      case Success(result, rest) =>
        read(rest, result :: acc)
      case _ =>
        val restString = text.pos.longString
        (acc.reverse, restString.drop(text.offset))
        
    }
  }



}
