package hexasignal.pico

import scala.util.parsing.combinator._
import scala.util.parsing.input._

object PicoParser {
  import Pico._

  def parse(input:String) : Option[PicoExpr] = {
    import PicoReader._
    parseAll(pexpr, input) match {
      case Success(result, next) => pexprToPicoExpr(result)
      case Error(_, next) => None
    }
  }

  def pexprToPicoExpr(pexpr:PExpr) : Option[PicoExpr] = {
    pexpr match {
      case PInt(n) => PicoInt(n).toOption
      case PFloat(f) => PicoFloat(f).toOption
      case PString(s) => PicoString(s).toOption
      case PSymbol(s) => PicoSymbol(s).toOption
      case PTrue => PicoTrue.toOption
      case PFalse => PicoFalse.toOption
      case BracketList(list @ _*) =>
        list.foldLeft(Some(List[PicoExpr]()):Option[List[PicoExpr]] ) { (b, pexpr) =>
          pexprToPicoExpr(pexpr) match {
            case None => None
            case Some(picoexpr) => b.map { _ :+ picoexpr }
          }
        } map { PicoList(_:_*) }  
      case ParList(PSymbol("lambda"), ParList(args @_*), expr) =>
        for{
          a <- parlistArgsToPicoArgs(args.toList)
          e <- pexprToPicoExpr(expr)
        }  yield PicoLambda(a, e)
      case ParList(PSymbol("define"),PSymbol(name), expr) =>
        pexprToPicoExpr(expr) map { PicoDefine(name, _) }
      case ParList(PSymbol("define"), ParList(PSymbol(name), args @ _*), expr) =>
        for{
          a <- parlistArgsToPicoArgs(args.toList)
          e <- pexprToPicoExpr(expr)
        }  yield PicoDefine(name, PicoLambda(a, e))
      case ParList(PSymbol("if"),cond, thn) =>
        for{ c <- pexprToPicoExpr(cond)
             t <- pexprToPicoExpr(thn)
        }  yield PicoIf(c, t, None)
      case ParList(PSymbol("if"), cond, thn, els) =>
        for{ c <- pexprToPicoExpr(cond)
             t <- pexprToPicoExpr(thn)
             e <- pexprToPicoExpr(els)
        }  yield PicoIf(c, t, Some(e))
      case ParList(PSymbol("let"), BracketList(bindings @ _*), expr) =>
        bracketListToBindingMap(bindings.toList) match {
          case Some(bindingMap) => pexprToPicoExpr(expr) map {PicoLet(bindingMap, _)}
          case None => None
        }
      case ParList(func, expr @ _*) =>
        for{
          f <- pexprToPicoExpr(func)
          e <- toOptionSeq(expr map pexprToPicoExpr)
        } yield PicoApply(f, e:_*)
    }
  }

  def toOptionSeq[A](seqOption:Seq[Option[A]]):Option[Seq[A]] =
    seqOption.foldRight(Some(Seq()):Option[Seq[A]]) {
      case (Some(elm), Some(lst)) => Some(elm +: lst)
      case _  => None
    }
    
  

  def parlistArgsToPicoArgs(args:List[PExpr]) : Option[PicoArgs] =
    args match {
      case PSymbol(name)::Nil => Some(PicoArgs(PicoSymbol(name)))
      case PSymbol(name)::restArgs => parlistArgsToPicoArgs(restArgs) map {
        case PicoArgs(args @_*) => PicoArgs((PicoSymbol(name) +:  args) :_*)
      }
      case _ => None
    }

  def bracketListToBindingMap(bindings: List[PExpr]) : Option[BindingMap] =
    bindings match {
      case (PSymbol(name))::expr::Nil => pexprToPicoExpr(expr) map { e => BindingMap(Map(PicoSymbol(name) -> e)) }
      case (PSymbol(name))::expr::rest =>
        for{
          BindingMap(map) <-  bracketListToBindingMap(rest)
          e <- pexprToPicoExpr(expr)
        } yield BindingMap(map + (PicoSymbol(name) -> e))
      case _ => None
    }

}

object PicoReader extends RegexParsers {
  import Pico._

  override val skipWhitespace = false

  def pexpr = patom | parlist | bracketlist

  def parlist : Parser[ParList] = "("~> repsep(pexpr, whiteSpace) <~")" ^^ {
    ParList(_:_*)
  }

  def bracketlist: Parser[BracketList] = "["~> repsep(pexpr, whiteSpace) <~ "]" ^^ { BracketList(_:_*) }

  def patom : Parser[PAtom] = pnumber | pstring | pbool | psymbol
 
  def psymbol : Parser[PSymbol] = """[^\s\(\)\[\]]+""".r ^^ {x => PSymbol(x)}

  def pstring : Parser[PString] = "\""~> """[^\"]*""".r <~"\""  ^^ {x => PString(x)}

  def pnumber : Parser[PNumber] = pfloat | pint
  
  def pint : Parser[PInt] = """-?\d+""".r ^^ { x => PInt(x.toInt)  }

  def pfloat : Parser[PFloat] = """-?\d+\.(\d)*""".r ^^ { x => PFloat(x.toFloat) }

  def pbool : Parser[PBool] = ptrue | pfalse

  def ptrue : Parser[PBool] = "true" ^^ { _ => PTrue }

  def pfalse : Parser[PBool] = "false" ^^ { _ => PFalse}
}
