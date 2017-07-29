package hexasignal.view


import scala.util.parsing.combinator._
import scala.util.parsing.input.{ Reader, Position }
import scala.math.{asin, acos, Pi}


case class Vector2(x: Double, y: Double) {
  def +(o: Vector2) = Vector2(x + o.x, y + o.y)
  def -(o: Vector2) = Vector2(x - o.x, y - o.y)
  def *(f: Double) = Vector2(x * f, y * f)
  def *(o: Vector2) = x * o.x + y * o.y
  def /(f: Double) = this * (1 / f)

  def magnitude = math.sqrt(x * x + y * y)
  def normalize = this / magnitude

  def angle = math.atan2(y, x)
}

class GestureReader( val data: List[Vector2], override val offset: Int ) extends Reader[Vector2] {
  def this( data: List[Vector2] ) = this( data, 0 )

  class GesturePosition( val offset: Int ) extends Position
  {
    override val line = 1
    override def column = offset + 1
    override def lineContents: String = ""
  }

  override def atEnd = offset >= (data.length - 1)
  override def first = data( offset )
  override def pos = new GesturePosition( offset )
  override def rest = new GestureReader( data, offset + 1 )
}





object Gesture{
  trait Atom {
    val pos : Vector2
  }
  object Atom {
    case class Idle(pos:Vector2) extends Atom
    case class Up(startPos:Vector2, pos:Vector2) extends Atom
    case class Down (startPos:Vector2, pos:Vector2) extends Atom
    case class Left (startPos:Vector2, pos:Vector2) extends Atom
    case class Right (startPos:Vector2, pos:Vector2) extends Atom
  }
}
case class Gesture(last:Gesture.Atom, history:List[Gesture.Atom])
case class Move(pos:Vector2, vel:Vector2, gesture: Gesture)

object GestureParser extends Parsers {
  import Gesture._
  import Atom._

  type Elem = Vector2

  def parse(list:List[Vector2]) : Option[Gesture] =
    list match {
      case Nil => None
      case head::rest =>
        gesture(Gesture(Idle(head),List()))(new GestureReader(rest)) match {
          case Success(g, _) => Some(g)
          case _ => None
        }
    }

  val minVel = 10
  val pos : Parser[Vector2] = elem("POS", e => true)


  def gesture2(g:Gesture) : Parser[Gesture] =
    (pos ~ gesture2(g) ^^ { case p1~g2 =>
       updateGesture(p1, g2)
     }) | pos ^^ { p => updateGesture(p, g) }

  def gesture(g:Gesture) : Parser[Gesture] =
    (up(g) | down(g) | left(g) | right(g) | idle(g)).flatMap(gesture) |
      (up(g) | down(g) | left(g) | right(g) | idle(g)) 

  def idle(g:Gesture) : Parser[Gesture] =
    (pos ^^ { p => (p,g)}).filter {
      case (p1,g) =>
        val last = g.last
        val p2 = last.pos
        val vel = p2 - p1
        vel.magnitude < minVel
    } ^^ {
      case (p1, g) =>
        val last = g.last
        val p2 = last.pos
        val history = g.history
        last match {
          case Idle(_) => Gesture(Idle(p1), history)
          case _ => Gesture(Idle(p1), last::history)
        }
    }

  def move(g:Gesture) : Parser[Move] = (
    (pos ^^ { p => (p,g)})
  ) ^^ {
    case (p1,g) =>
      val last = g.last
      val p2 = last.pos
      val vel = p2 - p1
      Move(p1, vel, g)
  } filter {
    case Move(p1, vel, g2) => vel.magnitude >= minVel
  }

  def up(g:Gesture) : Parser[Gesture] =
    move(g).filter { case Move(p1:Vector2, vel: Vector2, g:Gesture) =>
      val angle = vel.angle
      Pi/4 < angle && angle < Pi*3/4 
    } ^^ {
      case Move(p1, vel, g) =>
        val last = g.last
        val p2 = last.pos
        val history = g.history
        last match {
          case Up(startPos, _) => Gesture(Up(startPos, p1), history)
          case _ => Gesture(Up(p1, p1), last::history)
        }
    }


  def down(g:Gesture) : Parser[Gesture] =
    move(g).filter { case Move(p1:Vector2, vel: Vector2, g:Gesture) =>
      val angle = vel.angle
        -Pi/4 > angle && angle > -Pi*3/4 
    } ^^ {
      case Move(p1, vel, g) =>
        val last = g.last
        val p2 = last.pos
        val history = g.history
        last match {
          case Down(startPos, _) => Gesture(Down(startPos, p1), history)
          case _ => Gesture(Down(p1, p1), last::history)
        }
    }

  def left(g:Gesture) : Parser[Gesture] =
    move(g).filter { case Move(p1:Vector2, vel: Vector2, g:Gesture) =>
      val angle = vel.angle
      (Pi * 3/4 <= angle && angle <= Pi) || (-Pi <= angle && angle <= -Pi *3/4)
    } ^^ {
      case Move(p1, vel, g) =>
        val last = g.last
        val p2 = last.pos
        val history = g.history
        last match {
          case Left(startPos, _) => Gesture(Left(startPos, p1), history)
          case _ => Gesture(Left(p1, p1), last::history)
        }
    }


  def right(g:Gesture) : Parser[Gesture] =
    move(g).filter { case Move(p1:Vector2, vel: Vector2, g:Gesture) =>
      val angle = vel.angle
        -Pi/4 <= angle && angle <= Pi/4
    } ^^ {
      case Move(p1, vel, g) =>
        val last = g.last
        val p2 = last.pos
        val history = g.history
        last match {
          case Right(startPos, _) => Gesture(Right(startPos, p1), history)
          case _ => Gesture(Right(p1, p1), last::history)
        }
    }
 

  def updateGesture(p1: Vector2, g:Gesture) : Gesture = {
      val last = g.last
      val p2 = last.pos
      val history = g.history
      val vel = p2 - p1
      if( vel.magnitude < minVel ){
        Gesture(Idle(p2), last :: history)
        last match {
          case Idle(_) => Gesture(Idle(p1), history)
          case _ => Gesture(Idle(p1), last::history)
        }
      }else{
        val angle = vel.angle
        if( Pi/4 < angle && angle < Pi*3/4 ){
          last match {
            case Up(startPos, _) => Gesture(Up(startPos, p1), history)
            case _ => Gesture(Up(p1, p1), last::history)
          }
        }else if( -Pi/4 > angle && angle > -Pi*3/4  ){
          last match {
            case Down(startPos, _) => Gesture(Down(startPos, p1), history)
            case _ => Gesture(Down(p1, p1), last::history)
          }
        }else if( -Pi/4 <= angle && angle <= Pi/4  ){
          last match {
            case Right(startPos, _) => Gesture(Right(startPos, p1), history)
            case _ => Gesture(Right(p1, p1), last::history)
          }
        }else{
          last match {
            case Left(startPos, _) => Gesture(Left(startPos, p1), history)
            case _ => Gesture(Left(p1, p1), last::history)
          }
        }
      }
  }


}
