package hexasignal.pico

object Pico {
  sealed class PExpr
  
  case class ParList(list: PExpr*) extends PExpr
  case class BracketList(list:PExpr*) extends PExpr
  class PAtom extends PExpr

  case class PSymbol(name:String) extends PAtom
  class PNumber extends PAtom
  case class PString(string:String) extends PAtom
  class PBool extends PAtom

  case class PInt(i:Int) extends PNumber
  case class PFloat(f:Float) extends PNumber

  case object PTrue extends PBool
  case object PFalse extends PBool
}
