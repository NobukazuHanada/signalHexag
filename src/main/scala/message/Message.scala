package hexasignal.message


sealed abstract class Message
case class RectMessage(x : Double, y : Double, width : Double, height : Double) extends Message

