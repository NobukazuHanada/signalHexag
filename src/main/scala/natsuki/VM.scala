package hexasignal.natsuki


import hexasignal.natsuki.{Reader => NatsukiReader }
import Evaluator.RootEnv
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.Parsers

class VM {
  var text : String = ""
  var sexprs : Seq[SExpr] = Seq()
  var syntaxTrees : Seq[SyntaxTree] = Seq()

  val reader = new NatsukiReader()
  val parser = new NatsukiParser()
  val evaluator = new  Evaluator()
  val env = new RootEnv


  def run : Unit = {
    import reader._
    var noerror = true
    var next = new CharSequenceReader(text)
    sexprs = Seq()
    syntaxTrees = Seq()
   while( noerror && !next.atEnd  )
    parse(sexpr, new CharSequenceReader(text)) match {
      case Success(sexpr, next) =>
        sexprs :+= sexpr
        val parseResult = parser.parse(sexpr)
        parseResult match {
          case parser.Success(syntaxTree) =>
            syntaxTrees :+= syntaxTree
          case _ =>
            noerror = false
        }
      case Error(msg, next) =>
        noerror = false
      case Failure(msg, next) =>
        noerror = false
    }
   
  }



}


