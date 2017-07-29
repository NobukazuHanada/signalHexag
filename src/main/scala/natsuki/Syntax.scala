package hexasignal.natsuki

import scala.annotation.tailrec

sealed abstract class SyntaxTree(val sourceExpr: SExpr) extends Range{
  pos = sourceExpr.pos
  endPos = sourceExpr.endPos
}


case class True(override val sourceExpr: SExpr) extends SyntaxTree(sourceExpr)
case class False(override val sourceExpr: SExpr) extends SyntaxTree(sourceExpr)
case class Num[S <: Atom.Num](s: S) extends SyntaxTree(s)
case class Str(s:Atom.Str) extends SyntaxTree(s)

case class Symbol(s: Atom.Sym) extends SyntaxTree(s)

case class Var(s : Atom.Var) extends SyntaxTree(s)

case class ConsList(list:List[SyntaxTree], override val sourceExpr : SExpr) extends SyntaxTree(sourceExpr)

case class Func(name : Symbol, args : List[Var], prog: List[SyntaxTree], override val sourceExpr : SExpr) extends SyntaxTree(sourceExpr)
case class Define(left: SyntaxTree, right: SyntaxTree, override val sourceExpr : SExpr) extends SyntaxTree(sourceExpr)
case class If(cond: SyntaxTree, thenProg : SyntaxTree, elseProg : Option[SyntaxTree], override val sourceExpr : SExpr) extends SyntaxTree(sourceExpr)

case class Let(binding: Binding, prog: List[SyntaxTree], override val sourceExpr : SExpr) extends SyntaxTree(sourceExpr)
case class Binding(bindings: Map[Var, SyntaxTree], override val sourceExpr : SExpr) extends SyntaxTree(sourceExpr)
case class Apply(right: SyntaxTree, left : List[SyntaxTree], override val sourceExpr : SExpr) extends SyntaxTree(sourceExpr)
case class SetVar(v:Var, value: SyntaxTree, override val sourceExpr: SExpr) extends SyntaxTree(sourceExpr)



class NatsukiParser {
  val FN = Atom.Sym("fn")
  val DEFINE = Atom.Sym("define")
  val IF = Atom.Sym("if")
  val LET = Atom.Sym("LET")
  val ARROW = Atom.Sym("->")
  val SET = Atom.Sym("set")

  sealed abstract class Result[+S <: SyntaxTree]{
    def map[T <: SyntaxTree](f : S => T) : Result[T] =
      this match {
        case Success(s) => Success(f(s))
        case Error(e) => Error[T](e)
      }

    def flatMap[T <: SyntaxTree](f : S => Result[T]) : Result[T] =
      this match {
        case Success(s) => f(s)
        case Error(e) => Error[T](e)
      }
  }

  case class Success[+S <: SyntaxTree](s: S) extends Result[S]
  case class Error[+S <: SyntaxTree](msg:String) extends Result[S]

  implicit class ResultSyntaxTree[S <: SyntaxTree](s: S) {
    def toResult : Result[S] = Success(s)
  }

  implicit class listResults(results: List[Result[SyntaxTree]]) {
    def toResult(acc : List[SyntaxTree] = List())  : Either[List[SyntaxTree], Error[SyntaxTree]] =
      results match {
        case List() =>
          Left(acc.reverse)
        case Success(s) :: rest =>
          rest.toResult(s :: acc)
        case (e@Error(_)) :: rest =>
          Right[List[SyntaxTree], Error[SyntaxTree]](e)
      }

  }


  def parse(s : SExpr) : Result[SyntaxTree] =
    s match {
      case Atom.Bool.True => True(s).toResult
      case Atom.Bool.False => False(s).toResult
      case s:Atom.Sym => Symbol(s).toResult
      case v:Atom.Var => Var(v).toResult
      case i:Atom.Num.I => Num[Atom.Num.I](i).toResult
      case d:Atom.Num.D => Num[Atom.Num.D](d).toResult
      case r:Atom.Num.R => Num[Atom.Num.R](r).toResult
      case s:Atom.Str => Str(s).toResult
      case expr@DataList(list) =>
        list.map(parse(_)).toResult() match {
          case Left(a) => ConsList(a, expr).toResult
          case Right(e) => e
        }
      case expr@EvalList(
        FN :: (name@Atom.Sym(_)) :: functions
      ) =>
        createFunction(Symbol(name), functions, expr)
      case expr@EvalList(
        DEFINE :: left :: right :: List()
      ) =>
        for {
          l <- parse(left)
          r <- parse(right)
        } yield Define(l, r, expr)
      case expr@EvalList(
        IF :: cond :: thenExpr :: Nil
      ) =>
        for {
          c <- parse(cond)
          t <- parse(thenExpr)
        } yield If(c, t, None, expr)

      case expr@EvalList(
        IF :: cond :: thenExpr :: elseExpr :: Nil
      ) =>
        for {
          c <- parse(cond)
          t <- parse(thenExpr)
          e <- parse(elseExpr)
        } yield If(c, t, Some(e), expr)

      case expr@EvalList(
        LET :: (bindingList@DataList(_)) :: programs
      ) =>
        toBindingList(bindingList).flatMap( bindings =>
          programs.map(parse(_)).toResult() match {
            case Left(syntaxTrees) =>
              Let(bindings, syntaxTrees, expr).toResult
            case Right(e) =>
              e
          }
        )
      case expr@EvalList(
        SET :: (v@Atom.Var(_)) :: value :: Nil
      ) =>
        for {
         valueSyntax <- parse(value) 
        } yield SetVar(Var(v), valueSyntax, expr)
      case expr@EvalList(
        func :: programs
      ) =>
        parse(func) flatMap { funcSyntaxTree =>
          programs.map(parse(_)).toResult() match {
            case Left(e) => Apply(funcSyntaxTree, e, expr).toResult
            case Right(e) => e
          }
        }

      case e => error("can't parse",e)
    }

  def toBindingList(bindingDataList: DataList) : Result[Binding] = {
    def toBindingListIter(bindingList: List[SExpr], acc: Map[Var, SyntaxTree]) : Result[Binding] =
      bindingList match {
        case List() => Binding(acc, bindingDataList).toResult
        case (v@Atom.Var(_)) :: expr :: rest =>
          val syntax = parse(expr)
          syntax.flatMap( syntaxTree =>
            toBindingListIter(rest, acc + (Var(v) -> syntaxTree))
          )
        case _ => error("can't parse", bindingDataList)
    }
    toBindingListIter(bindingDataList.list, Map())
  }

  def createFunction(name: Symbol ,expr: List[SExpr], sourceExpr: SExpr) : Result[SyntaxTree] =
  {
    
    def iter(expr: List[SExpr], acc : List[Var]) : Result[SyntaxTree] =
      expr match {
        case Nil =>
          error("argument error", sourceExpr)
        case ARROW :: restPrograms =>
          val args = acc.reverse
          (restPrograms.map(parse _).toResult()) match {
            case Left(syntaxTrees) => Func(name, args, syntaxTrees, sourceExpr).toResult
            case Right(e) => e
          }
        case v::rest =>
          v match {
            case v:Atom.Var => iter(rest, Var(v)::acc)
            case _ => error("argument must be Var", v)
          }
      }
    iter(expr, List())
  }



  def error(msg: String, s: SExpr) =
    Error(s"Error : ${msg} from ${s.pos} to ${s.endPos} errorObj ${s.toString()}")

}
