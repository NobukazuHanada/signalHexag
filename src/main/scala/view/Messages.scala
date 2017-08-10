package hexasignal.view


abstract class Message

object Message {

  case object MBang extends Message
  case class MInt(i: Int) extends Message
  case class MRational(n:Int, d:Int) extends Message
  case class MDouble(d: Double) extends Message
  case class MString(s: String) extends Message
  case class MBool(b: Boolean) extends Message
  case class MList(lst: List[Message]) extends Message
  case class MObject(b: Map[String, Message] ) extends Message
  import hexasignal.natsuki.{SExpr, FilterSExpr}
  case class FilterCode(filter: FilterSExpr) extends Message
  case class MCode(code: SExpr,
                   meta: Map[String, SExpr]
  ) extends Message
}
