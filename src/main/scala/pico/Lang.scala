package hexasignal.pico

object Pico {

  case class PicoSentence(expr:PicoExpr*)
  sealed class PicoExpr 
  case class PicoInt(n:Int) extends PicoExpr
  case class PicoFloat(f:Float) extends PicoExpr
  case class PicoString(s:String) extends PicoExpr
  case class PicoSymbol(name:String) extends PicoExpr
  case object PicoTrue extends PicoExpr
  case object PicoFalse extends PicoExpr
  case class PicoList(list:PicoExpr*) extends PicoExpr
  case class PicoLambda(args:PicoArgs, expr:PicoExpr) extends PicoExpr
  case class PicoDefine(name:String, expr:PicoExpr) extends PicoExpr
  case class PicoDefineLambda(name:String, args:PicoArgs, expr:PicoExpr) extends PicoExpr
  case class PicoIf(cond:PicoExpr, thn:PicoExpr, els:Option[PicoExpr]) extends PicoExpr
  case class PicoLet(pico:BindingMap, expr:PicoExpr*) extends PicoExpr
  case class PicoApply(pico:PicoExpr, expr:PicoExpr*) extends PicoExpr

  case class PicoArgs(args:PicoSymbol*)
  case class BindingMap(map:Map[PicoSymbol, PicoExpr])

  case class PSentence(expr:PExpr*)

  sealed class PExpr
  case class ParList(list: PExpr*) extends PExpr
  case class BracketList(list:PExpr*) extends PExpr
  sealed class PAtom extends PExpr

  case class PSymbol(name:String) extends PAtom
  sealed class PNumber extends PAtom
  case class PString(string:String) extends PAtom
  sealed class PBool extends PAtom

  case class PInt(i:Int) extends PNumber
  case class PFloat(f:Float) extends PNumber

  case object PTrue extends PBool
  case object PFalse extends PBool

}
