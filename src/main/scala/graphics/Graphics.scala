package hexasignal.graphics

sealed abstract class Graphics

import hexasignal.pico.Runner._
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.{Color => FxColor}

case class Rect(x:Double ,y:Double ,w: Double,h:Double, fillColor : Color, strokeColor: Color) extends  Graphics
case class Line(x1:Double, y1:Double, x2:Double, y2:Double, strokeColor:Color) extends Graphics
case class Oval(x:Double, y:Double, w:Double, h:Double, fillColor: Color, strokeColor: Color) extends Graphics
case class Polygon(points: Seq[(Double, Double)], fillColor:Color, strokeColor:Color) extends Graphics
case class Group(x:Double, y:Double, graphics:Seq[Graphics]) extends Graphics

sealed abstract class Color
object NoColor extends Color
case class RGBA(r:Double, g:Double, b:Double, a:Double) extends Color
case class HSBA(h:Double, s:Double, b:Double, a:Double) extends Color


object Graphics {

  def draw(g:Graphics, context:GraphicsContext) : Boolean = {
    g match {
      case Rect(x,y,h,w,fill,stroke) =>
        colorSetup(fill, stroke, context)
        context.fillRect(x,y,w,h)
        context.strokeRect(x, y, w, h)
      case Line(x1,y1,x2,y2,stroke) =>
        colorSetup(NoColor, stroke, context)
        context.strokeLine(x1, y1, x2, y2)
      case Oval(x,y,w,h,fill,stroke) =>
        colorSetup(fill, stroke, context)
        context.fillOval(x, y, w, h)
        context.strokeOval(x, y, w, h)
      case Polygon(points, fill, stroke) =>
        colorSetup(fill, stroke, context)
        context.fillPolygon(points)
        context.strokePolygon(points)
      case Group(x,y, graphics) =>
        context.translate(x, y)
        graphics.foreach(draw(_, context))
        context.translate(-x, -y)
    }
    true
  }

  private def colorSetup(fill: Color, stroke: Color, context: GraphicsContext) : Boolean = {
    val fxFill = ConvertFXColorFrom(fill)
    val fxStroke = ConvertFXColorFrom(stroke)
    context.setFill(fxFill)
    context.setStroke(fxStroke)
    true
  }


  private def ConvertFXColorFrom(color:Color) : FxColor = {
    color match {
      case NoColor => FxColor(0, 0, 0, 0)
      case RGBA(r,g,b,a) => FxColor(r,g,b,a)
      case HSBA(h,s,b,a) => FxColor.hsb(h, s, b, a)
    }
  }
}

object GraphicFunctions {

  val rect : PartialFunction[Seq[Entity],Rect] =  {
    case Seq(EntNumber(x), EntNumber(y), EntNumber(w), EntNumber(h), EntForeignValue(f:Color), EntForeignValue(s:Color)) =>
      Rect(x,y,w,h,f,s)
  }

  val line : PartialFunction[Seq[Entity], Line] =  {
    case Seq(EntNumber(x1), EntNumber(y1), EntNumber(x2), EntNumber(y2), EntForeignValue(f:Color)) =>
      Line(x1,y1,x2,y2,f)
  }

  val oval : PartialFunction[Seq[Entity], Oval] =  {
    case Seq(EntNumber(x), EntNumber(y), EntNumber(w), EntNumber(h), EntForeignValue(f:Color), EntForeignValue(s:Color)) =>
      Oval(x,y,w,h,f,s)
  }

  val group : PartialFunction[Seq[Entity], Group] =
  {
    case Seq(EntNumber(x), EntNumber(y), EntList(seq @_*)) =>
      val graphics = seq collect { case EntForeignValue(g:Graphics) => g }
      Group(x,y, graphics)
  }

  val rgba : PartialFunction[Seq[Entity], Color] = {
    case Seq(EntNumber(x), EntNumber(y), EntNumber(w), EntNumber(h)) =>
      RGBA(x,y,w,h)
  }

  val hsba : PartialFunction[Seq[Entity], Color] = {
    case Seq(EntNumber(x), EntNumber(y), EntNumber(w), EntNumber(h)) =>
      HSBA(x,y,w,h)
  }

}

class Drawer(val context: GraphicsContext) {

  val draw : PartialFunction[Seq[Entity], EntValue] = {
    case Seq(EntForeignValue(g:Graphics)) =>
      context.setFill(FxColor.Black)
      context.clearRect(0,0,500,500)
      context.fillRect(0,0,500,500)
      Graphics.draw(g, context)
      EntTrue
  }
}
