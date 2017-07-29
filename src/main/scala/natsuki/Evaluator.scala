package hexasignal.natsuki

object Evaluator{
  sealed abstract class Value(val source : SyntaxTree)
  sealed abstract class WaitValue(source: SyntaxTree) extends Value(source) {
    val waitValues : Set[WaitValue]
  }

  sealed abstract class NoWaitValue(source: SyntaxTree) extends Value(source)

  case class ETrue(override val source: SyntaxTree) extends NoWaitValue(source)
  case class EFalse(override val source: SyntaxTree) extends NoWaitValue(source)
  case class EInt(i:Int, override val source: SyntaxTree) extends NoWaitValue(source)
  case class EDouble(i:Double, override val source:SyntaxTree) extends NoWaitValue(source)
  case class ERational(numer:Int, demon:Int, override val source: SyntaxTree) extends NoWaitValue(source)
  case class EStr(str:String, override val source:SyntaxTree) extends NoWaitValue(source)
  case class EList(list:List[NoWaitValue], override val source: SyntaxTree) extends NoWaitValue(source)
  case class ESymbol(name:String,  override val source: SyntaxTree) extends NoWaitValue(source)

  abstract class EFunc(override val source : SyntaxTree) extends NoWaitValue(source)
  case class Fn(args: List[Var], programs: List[SyntaxTree], env : Environment, override val source: SyntaxTree) extends EFunc(source)

  case class EmptyBox(name:Name, override val source: SyntaxTree ) extends WaitValue(source) {
    val waitValues : Set[WaitValue] = Set(this)
  }
  case class WaitList(list:List[Value], waitValues: Set[WaitValue], override val source : SyntaxTree) extends WaitValue(source)
  case class WaitApply(fn : Value, args : List[Value], waitValues : Set[WaitValue], override val source : SyntaxTree ) extends WaitValue(source)
  case class WaitIf(cond: Value, thenExpr : SyntaxTree, elseExpr : Option[SyntaxTree], waitValues : Set[WaitValue], override val source: SyntaxTree ) extends WaitValue(source)


  abstract class Name {
    val name : String
  }

  case class SymbolName private (symbolName:String) extends  Name{
    val name = symbolName
  }
  case class VarName private (varName:String) extends Name{
    val name = varName
  }
  object Name {
    def apply(symbol: Symbol) : SymbolName =
      SymbolName(symbol.s.name)

    def apply(v: Var) : VarName =
      VarName(v.s.name)
  }


  abstract class Environment{

    var variableTable : Map[VarName, Value] = Map()
    var funcSymbolTable : Map[SymbolName, Value] = Map()

    def createChild : Environment =
      new ChildEnv(this)

    def get(n: VarName) : Option[Value] =
      variableTable get n

    def get(n: SymbolName) : Option[Value] =
      funcSymbolTable get n match {
        case Some(func) => Some(func)
        case None => None
      }

    def set(n: VarName, c: Value) : Environment = {
      variableTable = variableTable + (n -> c)
      this
    }

    def set(n: SymbolName, c : Value ) = {
      funcSymbolTable = funcSymbolTable + (n -> c)
      this
    }
  }

  class RootEnv extends Environment

  class ChildEnv(val parent: Environment) extends Environment{
    override def get(n: VarName) : Option[Value] =
      (variableTable get n) match {
        case Some(n) => Some(n)
        case None => parent get n 
      }


    override def get(n: SymbolName) : Option[Value] =
      (funcSymbolTable get n) match {
        case Some(n) => Some(n)
        case None => parent get n
      }
  }

}

class Evaluator {
  import Evaluator._

  var stopExprs : Map[Var,List[SyntaxTree]] = Map()
  var funcMap : Map[ESymbol, Fn] = Map()


  sealed abstract class Result[+S <: Value]
  case class Success[+S <: Value](s:S) extends Result[S]
  case class Error[+S <: Value](msg:String)  extends Result[S]

  implicit class ResultValue[+V <: Value](v: V) {
    def toResult(implicit environment: Environment) : (Result[V], Environment) =
      (Success(v), environment)
  }

  def eval(s:SyntaxTree, env : Environment) : (Result[Value], Environment) = {
    implicit val enviroment = env
    s match {
      case (syntax@Num(s)) =>
        s match {
          case Atom.Num.I(i) => EInt(i,syntax).toResult
          case Atom.Num.D(d) => EDouble(d, syntax).toResult
          case Atom.Num.R(n,d) => ERational(n, d, syntax).toResult
        }
      case (t@True(_)) =>
        ETrue(t).toResult
      case (f@False(_)) =>
        EFalse(f).toResult 
      case str@Str(Atom.Str(s)) =>
        EStr(s, str).toResult
      case sym@Symbol(Atom.Sym(name)) =>
        ESymbol(name, sym).toResult
      case exprV@Var(v) =>
        val name = Name(exprV)
        env get name match {
          case Some(EmptyBox(name, _)) =>  EmptyBox(name,exprV).toResult
          case Some(value) => value.toResult
          case None =>
            val emptyBox = EmptyBox(name, exprV)
            env.set(name, emptyBox)
            emptyBox.toResult
        }
      case list@ConsList(lst, _) =>
        var noWaitValues : List[NoWaitValue] = List()
        var waitValueSet : Set[WaitValue] = Set()
        var values : List[Value]  = List()
        var err : Option[Error[Value]] = None
        for(l <- lst ) {
          err match {
            case Some(e) => 
            case None =>
              eval(l, env)._1 match {
                case e@Error(_) =>
                  err = Some(e)
                case Success(waitValue:WaitValue) =>
                  waitValueSet += waitValue
                  values :+= waitValue
                case Success(noWaitValue:NoWaitValue) =>
                  noWaitValues :+= noWaitValue
                  values :+= noWaitValue
              }
          }
        }
        err match {
          case None =>
            if( waitValueSet.isEmpty ){
              EList(noWaitValues, list).toResult
            }else{
              WaitList(values, waitValueSet, list).toResult
            }
          case Some(e) => (e, env)
        }
      case func@Func(sym@Symbol(Atom.Sym(name)), args, prog, sourceExpr) =>
        val fn = Fn(args, prog, env, func)
        val n = Name(sym)
        enviroment.set(n, fn)
        fn.toResult
      case syntaxTree@If(cond, thenProg, elseProg, sourceExpr) =>
        val (result, _) = eval(cond, env)
        result match {
          case Success(ETrue(_)) => eval(thenProg, env)
          case Success(EFalse(_)) =>
            elseProg match {
              case Some(elseP) => eval(elseP, env)
              case None => EFalse(syntaxTree).toResult
            }
          case Success(e:WaitValue) =>
            WaitIf(e, thenProg, elseProg, e.waitValues, syntaxTree).toResult
          case Success(_) => (error("condition must be boolean", cond), env)
          case e@Error(_) => (e, env)
        }
      case let@Let(binding, progs, sourceExpr) =>
        val childEnv = enviroment.createChild
        var err : Option[Error[Value]] = None
        for((v, expr) <- binding.bindings){
          err match {
            case Some(e) => 
            case None =>
              val (result,_) = eval(expr, env)
              result match {
                case e@Error(_) =>
                  err = Some(e)
                case Success(value) =>
                  childEnv.set(Name(v), value)
              }
          }
        }
        err match {
          case Some(e) => (e, env)
          case None =>
            val initProg = progs.init
            val lastProg = progs.last
            initProg.foreach(eval(_,childEnv))
            val (result, _) = eval(lastProg, childEnv)
            (result, env)
        }
      case b@Binding(_, _) =>
        (error("bindings must be in let", b), env)
      case define@Define(left, right, sourceExpr) =>
        left match {
          case v@Var(_) =>
            val (result, _ ) = eval(right, env)
            result match {
              case e@Error(_) => (e, env)
              case Success(value) =>
                env.set(Name(v), value)
                ETrue(define).toResult
            }
          case _ => (error("define failed", define), env)
        }
      case SetVar(v, valueSyntax, _) =>
        val value = eval(valueSyntax, env)._1
        value match  {
          case e@Error(_) => (e, env)
          case Success(successValue) =>
            env.set(Name(v), successValue)
            (value, env)
        }
      case apply@Apply(func, args, sourceExpr) =>
        func match {
          case sym@Symbol(_) =>
            val name = Name(sym)
            (env get name) match {
              case None =>
                val fn = EmptyBox(name, func)
                env.set(name, fn)
                parses(args, env,
                       (values) => Success(WaitApply(fn, values, Set(fn), apply)),
                       (values, waitValueSet) => Success(WaitApply(fn, values, waitValueSet + fn, apply))
                )
              case Some(f) =>
                f match {
                  case fn:EmptyBox =>
                    env.set(name, fn)
                    parses(args, env,
                           (values)=> Success(WaitApply(fn, values, Set(fn), apply)),
                           (values, waitValues) => Success(WaitApply(fn, values, waitValues + fn, apply))
                    )
                  case fn:EFunc =>
                    parses(args, env, 
                           (values) => {
                             fn match {
                               case Fn(args, programs, env1, source) =>
                                 if(args.length <= values.length ) {
                                   val childEnv = env1.createChild
                                   for((a,v) <- args.zip(values)) {
                                     childEnv.set(Name(a), v)
                                   }
                                   val initProg = programs.init
                                   val lastProg = programs.last
                                   initProg.foreach(eval(_,childEnv))
                                   val (result, _) = eval(lastProg, childEnv)
                                   result
                                 }else{
                                   val childEnv = env1.createChild
                                   for((a,v) <- args.zip(values)) {
                                     childEnv.set(Name(a), v)
                                   }
                                   val restArgs = args.drop(values.length)
                                   Success(Fn(restArgs, programs, childEnv, apply))
                                 }
                             }
                           },
                           (values, waitValues) => Success(WaitApply(fn, values, waitValues, apply))
                    )
                  case _ =>  (error("func  is symbol", func), env)
                }
            }
          case _ => (error("func  is symbol", func), env)
        }
    }
  }




  def parses(syntaxTrees: List[SyntaxTree], env : Environment,
             whenGetAllNoWaitValues: List[NoWaitValue] => Result[Value],
             whenGetSomeWaitValue: (List[Value], Set[WaitValue]) => Result[Value]
  ) : (Result[Value], Environment) = {
    var noWaitValues : List[NoWaitValue] = List()
    var waitValueSet : Set[WaitValue] = Set()
    var values : List[Value]  = List()
    var err : Option[Error[Value]] = None
    for(l <- syntaxTrees ) {
      err match {
        case Some(e) =>
        case None =>
          eval(l, env)._1 match {
            case e@Error(_) =>
              err = Some(e)
            case Success(waitValue:WaitValue) =>
              waitValueSet += waitValue
              values :+= waitValue
            case Success(noWaitValue:NoWaitValue) =>
              noWaitValues :+= noWaitValue
              values :+= noWaitValue
          }
      }
    }
    err match {
      case Some(e) => (e, env)
      case None =>
        if( waitValueSet.isEmpty ){
          (whenGetAllNoWaitValues(noWaitValues),env)
        }else{
          (whenGetSomeWaitValue(values, waitValueSet), env)
        }
    }
  }



  def error(msg: String, s: SyntaxTree) =
    Error(s"Error : ${msg} from ${s.pos} to ${s.endPos} errorObj ${s.toString()}")

}

