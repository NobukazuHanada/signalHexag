package hexasignal.model

import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.control.TextArea
import scalafx.scene.input.{KeyEvent, KeyCode}
import IDGenerator.Id

class ViewModelEditor(val model : ViewModel) extends Scene with Watcher{
  val textArea = new TextArea("") {

  }

  val r = new scala.util.Random()

  def safeStringToInt(str: String): Option[Int] = {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toInt
  }

  private def nullThenRandom(s:String, default : Int = 500) : Int = 
    safeStringToInt(s) match {
      case None => r.nextInt(default)
      case Some(i) => i
    }
  

  def data : Map[Id, ViewData] = {
    val text = textArea.text()
    var data : Map[Id, ViewData] = Map()
    val (sexprs,rest) = Compile.read(text)

    for( Compile.SList(sexpr) <- sexprs ) {
      import Compile.Number
      val default = 500
      val random = (() => r.nextInt(default))
      val viewData =
        sexpr match {
          case List(Compile.Rect) =>
            Rect(random(), random(),random(), random(), Color.Black, Color.White)
          case List(Compile.Rect,Number(x)) =>
            Rect(x, random(), random(), random(), Color.Black, Color.White)
          case List(Compile.Rect,Number(x),Number(y)) =>
            Rect(x,y,random(),random(), Color.Black, Color.White)
          case List(Compile.Rect,Number(x),Number(y), Number(w)) =>
            Rect(x,y,w,random(), Color.Black, Color.White)
          case List(Compile.Rect,Number(x),Number(y), Number(w), Number(h)) =>
            Rect(x,y,w,h, Color.Black, Color.White)
          case List(Compile.Line) =>
            Line(random(),random(),random(),random(), Color.White)
          case List(Compile.Line, Number(x1)) =>
            Line(x1,random(), random(), random(), Color.White)
          case List(Compile.Line, Number(x1), Number(y1)) =>
            Line(x1,y1,random(),random(), Color.White)
          case List(Compile.Line, Number(x1), Number(y1), Number(x2)) =>
            Line(x1,y1,x2,random(),Color.White)
          case List(Compile.Line, Number(x1), Number(y1), Number(x2), Number(y2)) =>
            Line(x1,y1,x2,y2, Color.White)
          case _ =>
            Undefined
        }

      data += (IDGenerator.generate -> viewData)
    }
    data
  }

  
 
  def noticed() {
    var texts : List[String] = List()
    val viewDatas = model.dataModel.toList
    for( (_, viewData) <- viewDatas ) {
      viewData match {
        case Rect(x,y,w,h,_,_) =>
          texts :+= (List("rect",
                       x.toInt.toString(),
                       y.toInt.toString(),
                       w.toInt.toString(),
                          h.toInt.toString()) mkString("("," ", ")"))

        case Line(x1,y1,x2,y2,_) =>
          texts :+= (List("line",
                          x1.toInt.toString(),
                          y1.toInt.toString(),
                          x2.toInt.toString(),
                          y2.toInt.toString()) mkString("("," ", ")"))
        case _ =>
      }
    }
    textArea.text() = (texts mkString "\n") + "\n"
    textArea.positionCaret(textArea.text().length())
  }

  textArea.handleEvent(KeyEvent.KeyPressed) {
    (event: KeyEvent) =>
    if( event.code == KeyCode.Enter  ){
      model.setDataModel(data)
      event.consume()
    }
  }
 
  noticed()
  model.addWatcher(this)
  content = textArea
}
