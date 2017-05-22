package hexasignal.pico

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import Result._

object PicoParser {
  import Pico._

  def parse(input:String) : Result[PicoSentence] = {
    import PicoReader._
    parseAll(psentence, input) match {
      case Success(result, next) => psentenceToPicoSentence(result)
      case NoSuccess(msg, next) => ReadError(msg)
    }
  }

  def psentenceToPicoSentence(expr:PSentence) : Result[PicoSentence] =
    expr match {
      case PSentence() => Success(PicoSentence())
      case PSentence(expr) => pexprToPicoExpr(expr) map { PicoSentence(_) }
      case PSentence(first, rest @_*)  =>
        for{
          first <- pexprToPicoExpr(first)
          PicoSentence(rest @_*) <- psentenceToPicoSentence(PSentence(rest:_*))
        } yield PicoSentence(first +: rest:_*)
    }

  def pexprToPicoExpr(pexpr:PExpr) : Result[PicoExpr] = {
    pexpr match {
      case PInt(n) => PicoInt(n).toResult
      case PFloat(f) => PicoFloat(f).toResult
      case PString(s) => PicoString(s).toResult
      case PSymbol(s) => PicoSymbol(s).toResult
      case PTrue => PicoTrue.toResult
      case PFalse => PicoFalse.toResult
      case BracketList(list @ _*) =>
        list.foldLeft(List[PicoExpr]().toResult:Result[List[PicoExpr]] ) { (b, pexpr) =>
          pexprToPicoExpr(pexpr) match {
            case Success(picoexpr) => b.map { _ :+ picoexpr }
            case error:Error => error
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
          case Success(bindingMap) => pexprToPicoExpr(expr) map {PicoLet(bindingMap, _)}
          case error:Error => error
        }
      case ParList(func, expr @ _*) =>
        for{
          f <- pexprToPicoExpr(func)
          e <- (expr map pexprToPicoExpr).toResultSeq
        } yield PicoApply(f, e:_*)
    }
  } 

  def parlistArgsToPicoArgs(args:List[PExpr]) : Result[PicoArgs] =
    args match {
      case PSymbol(name)::Nil => Success(PicoArgs(PicoSymbol(name)))
      case PSymbol(name)::restArgs => parlistArgsToPicoArgs(restArgs) map {
        case PicoArgs(args @_*) => PicoArgs((PicoSymbol(name) +:  args) :_*)
      }
      case _ => ParseError("parlistArgsToPicoArgs Error")
    }

  def bracketListToBindingMap(bindings: List[PExpr]) : Result[BindingMap] =
    bindings match {
      case (PSymbol(name))::expr::Nil => pexprToPicoExpr(expr) map { e => BindingMap(Map(PicoSymbol(name) -> e)) }
      case (PSymbol(name))::expr::rest =>
        for{
          BindingMap(map) <-  bracketListToBindingMap(rest)
          e <- pexprToPicoExpr(expr)
        } yield BindingMap(map + (PicoSymbol(name) -> e))
      case _ => ParseError("bracketListToBindingMap Error")
    }

}

object PicoReader extends RegexParsers {
  import Pico._

  override val skipWhitespace = false
  def psentence : Parser[PSentence] = repsep  (pexpr, whiteSpace) ^^ { PSentence(_:_*) }

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
