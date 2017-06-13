package hexasignal.pico

import Result._

object Runner {

  import Pico._
  sealed class Entity
  object Empty extends Entity
  sealed class EntValue extends Entity
  sealed class EntNumber extends EntValue
  object EntNumber {
    def unapply(d:EntNumber):Option[Double] =
    d match {
      case EntInt(i) => Some(i.toDouble)
      case EntFloat(f) => Some(f.toDouble)
    }
  }


  sealed class EntBool extends EntValue

  case class EntInt(n:Int) extends EntNumber
  case class EntFloat(f:Float) extends EntNumber
  case class EntString(s:String) extends EntValue
  case object EntTrue extends EntBool
  case object EntFalse extends EntBool
  case class EntSymbol(name:String) extends EntValue
  case class EntList(values:Entity*) extends EntValue
  case class EntLambda(args:PicoArgs, expr:Seq[PicoExpr], env: Environment, name: Option[String]) extends EntValue
  case class EntForeignValue[F](f:F) extends EntValue
  case class EntForeignFunc[F](f: PartialFunction[Seq[Entity], F]) extends EntValue


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
      case PicoLambda(args, expr @ _*) =>
        (EntLambda(args, expr, env, None), env).toResult
      case PicoDefine(name, exprs @ _*) =>
        var newEnv = env
        var result : Entity = null
        var error : Error = null 
        for( expr <- exprs ){
          if( error ==  null ){
            runExpr(expr, newEnv) match {
              case Success((r:Entity, e:Environment)) =>
                newEnv = e
                result = r
              case e:Error =>
                error = e
            }
          }
        }
        if( error == null )
          (result, env + (PicoSymbol(name) -> result) ).toResult
        else
          error
      case PicoDefineLambda(name, args, expr @ _*) =>
        val entLambda = EntLambda(args, expr, env, Some(name))
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
          case lambda@EntLambda(PicoArgs(args @_*), exprs, lambdaEnv, name) =>
            entities flatMap {
              entities => 
              var newEnv = name match {
                case Some(name) =>
                  lambdaEnv + (PicoSymbol(name) -> lambda)
                case None => lambdaEnv
              }
              for( (symbol, value) <- env.variableMap  ){
                newEnv = newEnv + (symbol -> value)
              }
              for((symbol, value) <- args.zip(entities)){
                newEnv += symbol -> value
              }
              var result : Entity = null
              for( expr <- exprs ){
                val Success((r:Entity,e:Environment)) = runExpr(expr, newEnv)
                result = r 
                newEnv = e
              }
              (result, env).toResult
            }
          case EntForeignFunc(f) =>
            entities flatMap {
              case entities =>
                if(f.isDefinedAt(entities)){
                  f(entities) match {
                    case entValue:EntValue => (entValue, env).toResult
                    case foreignValue =>
                      (EntForeignValue(foreignValue), env).toResult
                  }
                }else{
                  VMError(s"Apply Error funcEntity:$func entities:$entities" )
                }
            }
          case _ =>
            VMError(s"Apply Error funcEntity:$func entities:$entities" )
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
