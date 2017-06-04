package hexasignal.pico


object PicoVM {
}

class PicoVM(env: Environment) {
  import Pico._

  implicit private val environment = env
  private var _text: String = ""
  private var _ast: PicoSentence = PicoSentence()
  private var _env: Environment = env

  private var envEvents : List[Environment => Boolean] = List[Environment => Boolean]()

  def addFireEnvEvent(f: Environment => Boolean) = {
    envEvents :+= f
  }

  private def fireEnvEvents() : Boolean = {
    for( envEvent <- envEvents ){
      envEvent(_env)
    }
    true
  }

  def text: String = _text
  def getEnv: Environment = _env
  def ast: PicoSentence = _ast
  def text_=(input:String) =
  if( _text != input ) {
    _text = input
    PicoParser.parse(input) match {
      case Success(ast) =>
        _ast = ast
        _text = UnParser.unparse(ast)
        Runner.run(ast) match {
          case Success((_, env:Environment)) =>
            _env = env
            fireEnvEvents()
          case err =>
            println(err)
        }
      case _ => 
    }
  }

  def ast_(ast: PicoSentence) =
  if(ast != _ast){
    _text = UnParser.unparse(ast)
    Runner.run(ast) match {
      case Success((_, env:Environment)) =>
        _env = env
        fireEnvEvents()
      case _ =>
    }
  }

}
