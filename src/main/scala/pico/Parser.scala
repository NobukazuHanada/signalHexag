package hexasignal.pico

import scala.util.parsing.combinator._
import scala.util.parsing.input._

object PicoReader extends RegexParsers {
  import Pico._

  override val skipWhitespace = false

  def pexpr = patom | parlist | bracketlist

  def parlist : Parser[ParList] = "("~> repsep(pexpr, whiteSpace) <~")" ^^ {
    ParList(_:_*)
  }

  def bracketlist: Parser[BracketList] = "["~> repsep(pexpr, whiteSpace) <~ "]" ^^ { BracketList(_:_*) }

  def patom : Parser[PAtom] = pnumber | pstring | pbool | psymbol
 
  def psymbol : Parser[PSymbol] = """[^\s\(\)\[\]]+""".r ^^ {x => PSymbol(x)}

  def pstring : Parser[PString] = "\""~> """[^\"]*""".r <~"\""  ^^ {x => PString(x)}

  def pnumber : Parser[PNumber] = pfloat | pint
  
  def pint : Parser[PInt] = """-?\d+""".r ^^ { x => PInt(x.toInt)  }

  def pfloat : Parser[PFloat] = """-?\d+\.(\d)*""".r ^^ { x => PFloat(x.toFloat) }

  def pbool : Parser[PBool] = ptrue | pfalse

  def ptrue : Parser[PBool] = "true" ^^ { _ => PTrue }

  def pfalse : Parser[PBool] = "false" ^^ { _ => PFalse}
}
