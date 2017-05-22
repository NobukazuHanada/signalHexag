package hexasignal.pico

import Result._

object Runner {

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
  case class EntForeignValue[F](f:F) extends EntValue
  case class EntForeignFunc[F](f: Seq[Entity] => F) extends EntValue


  def run(program:PicoSentence)(implicit environment:Environment) : Result[(Entity, Environment)] = {
    program match {
      case PicoSentence() => (Empty, environment).toResult
      case PicoSentence(head) => runExpr(head, environment)
      case PicoSentence(head, rest @_*) =>
        runExpr(head, environment).flatMap{ case (_, env) =>run(PicoSentence(rest:_*))(env) }
    }
  }

  def runExpr(expr: PicoExpr, env: Environment) : Result[(Entity, Environment)] =
    expr match {
      case PicoInt(n) =>
        (EntInt(n), env).toResult
      case PicoFloat(f) =>
        (EntFloat(f), env).toResult
      case PicoString(s) =>
        (EntString(s), env).toResult
      case symbol@PicoSymbol(s) =>
        env.get(symbol) match {
          case Some(e) => (e,env).toResult
          case None => (EntSymbol(s),env).toResult
        }
      case PicoTrue =>
        (EntTrue, env).toResult
      case PicoFalse =>
        (EntFalse, env).toResult
      case PicoList(list @_*) =>
        val entValues = ( list map { elm => runExpr(elm, env) map (_._1) } ).toResultSeq
        entValues.map { (values) => (EntList(values:_*), env) }
      case PicoLambda(args, expr) =>
        (EntLambda(args, expr, env), env).toResult
      case PicoDefine(name, expr) =>
        runExpr(expr, env) map { case (entity, env) => (Empty, env + (PicoSymbol(name) -> entity)) }
      case PicoDefineLambda(name, args, expr) =>
        val entLambda = EntLambda(args, expr, env)
        (entLambda, env + (PicoSymbol(name) -> entLambda)).toResult
      case PicoIf(cond, thn, els) =>
        runExpr(cond, env) map (_._1) flatMap {
          case EntFalse =>
          els match {
            case Some(e) => runExpr(e, env)
            case None => (Empty, env).toResult
          }
          case _ => runExpr(thn, env)
        }
      case PicoLet(bindings, exprs @_*) =>
        bindingsToEnv(bindings, env) flatMap { newEnv => 
          exprs.foldRight((Empty:Entity,newEnv).toResult) {
            (expr, entitiAndEnv) =>
            entitiAndEnv.flatMap { case (_, env) => runExpr(expr, env) }
          } map { case (entity, _) => (entity, env) }
        }
      case PicoApply(funcExpr, expr @_*) =>
        val func = runExpr(funcExpr, env) map (_._1)
        val entities = (expr map { runExpr(_, env) map (_._1) }).toResultSeq
        func flatMap {
          case EntLambda(PicoArgs(args @_*), expr, env) =>
            entities flatMap {
              entities => 
              var newEnv = env
              for((symbol, value) <- args.zip(entities)){
                newEnv += symbol -> value
              }
              runExpr(expr, newEnv) map { case (result, _) =>  (result, env) }
            }
          case EntForeignFunc(f) =>
            entities flatMap {
              entities => 
              val foreignValue = f(entities);
              (EntForeignValue(foreignValue), env).toResult
            }
          case _ =>
            VMError("Apply Error")
        }
    }

  def bindingsToEnv(bindings:BindingMap, env: Environment) : Result[Environment] =
    bindings match {
      case BindingMap(letMap) =>
        letMap.foldRight(env.toResult:Result[Environment]) {
          case ((symbol, expr), letEnv) =>
            letEnv.flatMap {
              letEnv =>
              runExpr(expr, env) map (_._1) map {
                entity =>
                (letEnv + (symbol -> entity))
              }
            }
        }
    }
}
