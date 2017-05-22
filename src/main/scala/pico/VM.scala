package hexasignal.pico


object PicoVM {
}

class PicoVM(env: Environment) {
  import Pico._

  implicit private val environment = env
  private var _text: String = ""
  private var _ast: PicoSentence = PicoSentence()
  private var _env: Environment = env

  def text: String = _text
  def text_=(input:String) =
  if( _text != input ) {
    _text = input
    PicoParser.parse(input) match {
      case Success(ast) =>
        _ast = ast
        _text = UnParser.unparse(ast)
        Runner.run(ast) match {
          case Success((_, env:Environment)) => _env = env
          case _ => 
        }
      case _ => 
    }
  }

  def ast_(ast: PicoSentence) =
  if(ast != _ast){
    _text = UnParser.unparse(ast)
    Runner.run(ast) match {
      case Success((_, env:Environment)) => _env = env
      case _ =>
    }
  }

}
