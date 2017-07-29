package hexasignal.natsuki.editor

import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.control.TextArea
import scalafx.scene.input.{KeyEvent, KeyCode}
import scalafx.scene.input.MouseEvent
import hexasignal.natsuki.NatsukiParser
import hexasignal.natsuki.Evaluator
import hexasignal.natsuki.Evaluator.RootEnv
import hexasignal.natsuki.{Reader => NatsukiReader }
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.Reader 

class CodeEditor extends TextArea("") {
  self =>
  val reader = new NatsukiReader()
  val parser = new NatsukiParser()
  val evaluator = new  Evaluator()
  val env = new RootEnv

  minHeight = 600

  handleEvent(MouseEvent.MouseClicked) {
    (e:MouseEvent) =>
    parseExpr(new CharSequenceReader(text()))
  }

  def parseExpr(text: Reader[Char] ) : Unit = {
    import reader._
    parse(sexpr, text) match {
      case Success(sexpr, next) =>
        val parseResult = parser.parse(sexpr)
        parseResult match {
          case parser.Success(syntaxTree) =>
            val evalResult = evaluator.eval(syntaxTree, env)
            println(evalResult._1)
          case _ =>
        }
        if( !next.atEnd ){
          parseExpr(next.rest)
        }
        
      case Error(msg, next) =>
        println(msg)
      case Failure(msg, next) =>
        println(msg)
    }
  }

}
