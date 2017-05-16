package hexasignal.pico

import scala.util.parsing.combinator._
import scala.util.parsing.input._

class PicoVM {
  import Pico._
  import PicoVM._

  def run(program:PicoSentence, env:Environment) : (Entity, Environment) = {
    program match {
      case PicoSentence() => (Empty, env)
      case PicoSentence(head) => runExpr(head, env)
      case PicoSentence(head, rest @_*) => run(PicoSentence(rest:_*), runExpr(head, env)._2)
    }
  }

  def runExpr(expr: PicoExpr, env: Environment) : (Entity, Environment) = 
    expr match {
      case PicoInt(n) =>
        (EntInt(n), env)
      case PicoFloat(f) =>
        (EntFloat(f), env)
      case PicoString(s) =>
        (EntString(s), env)
      case symbol@PicoSymbol(s) =>
        env.get(symbol) match {
          case Some(e) => (e,env)
          case None => (EntSymbol(s),env)
        }
      case PicoTrue =>
        (EntTrue, env)
      case PicoFalse =>
        (EntFalse, env)
      case PicoList(list @_*) =>
        val entValues = list map { elm => runExpr(elm, env)._1 }
        (EntList(entValues:_*), env)
      case PicoLambda(args, expr) =>
        (EntLambda(args, expr, env), env)
      case PicoDefine(name, expr) =>
        val (entity, _) = runExpr(expr, env)
        (Empty, env + (PicoSymbol(name) -> entity))
      case PicoIf(cond, thn, els) =>
        runExpr(cond, env)._1 match {
          case EntFalse =>
          els match {
            case Some(e) => runExpr(e, env)
            case None => (Empty, env)
          }
          case _ => runExpr(thn, env)
        }
      case PicoLet(bindings, exprs @_*) =>
        val letEnv = bindingsToEnv(bindings, env)
        val (entity, _) = exprs.foldRight((Empty:Entity,letEnv)) {
          (expr, entitiAndEnv) =>
          runExpr(expr, entitiAndEnv._2)
        }
        (entity,env)
      case PicoApply(funcExpr, expr @_*) =>
        val func = runExpr(funcExpr, env)._1
        val entities = expr map { runExpr(_, env)._1 }
        func match {
          case EntLambda(PicoArgs(args @_*), expr, env) =>
            var newEnv = env
            for((symbol, value) <- args.zip(entities)){
              newEnv += symbol -> value
            }
            (runExpr(expr, newEnv)._1, env)
        }
    }


  def bindingsToEnv(bindings:BindingMap, env: Environment) : Environment =
    bindings match {
      case BindingMap(letMap) =>
        val eletMap = letMap.map { binds => (binds._1, runExpr(binds._2, env)._1) }
        var newEnv = env
        for((symbol,value) <- eletMap) {
          newEnv += (symbol -> value)
        }
        newEnv
    }

}

object PicoVM {
  import Pico._
  sealed class Entity
  object Empty extends Entity
  sealed class EntValue extends Entity

  case class EntInt(n:Int) extends EntValue
  case class EntFloat(f:Float) extends EntValue
  case class EntString(s:String) extends EntValue
  case object EntTrue extends EntValue
  case object EntFalse extends EntValue
  case class EntSymbol(name:String) extends EntValue
  case class EntList(values:Entity*) extends EntValue
  case class EntLambda(args:PicoArgs, expr:PicoExpr, env: Environment) extends EntValue

}

object Environment {
  import Pico._
  import PicoVM._
  def apply() : Environment = new Environment(Map[PicoSymbol, Entity]())

  def apply(maps:(PicoSymbol, Entity)*) : Environment = {
    val mutableMap = scala.collection.mutable.Map[PicoSymbol, Entity]()
    for(elem <- maps) {
      mutableMap += elem
    }
    new Environment(mutableMap.toMap)
  }

  def apply(map:Map[PicoSymbol, Entity]) = new Environment(map)
}


class Environment private (val varialbeMap: Map[Pico.PicoSymbol, PicoVM.Entity]) {
  import Pico._
  import PicoVM._

  def get(symbol:PicoSymbol) : Option[Entity] =
    varialbeMap.get(symbol)

  def +(cell:(PicoSymbol, Entity)) : Environment =
    Environment(varialbeMap + cell)
}

object PicoParser {
  import Pico._

  def parse(input:String) : Option[PicoSentence] = {
    import PicoReader._
    parseAll(psentence, input) match {
      case Success(result, next) => psentenceToPicoSentence(result)
      case _ => None
    }
  }

  def psentenceToPicoSentence(expr:PSentence) : Option[PicoSentence] =
    expr match {
      case PSentence() => None
      case PSentence(expr) => pexprToPicoExpr(expr) map { PicoSentence(_) }
      case PSentence(first, rest @_*)  =>
        for{
          first <- pexprToPicoExpr(first)
          PicoSentence(rest @_*) <- psentenceToPicoSentence(PSentence(rest:_*))
        } yield PicoSentence(first +: rest:_*)
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
