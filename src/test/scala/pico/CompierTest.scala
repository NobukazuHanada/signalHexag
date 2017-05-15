package hexasignal.pico

import org.scalatest.FunSuite

class PicoReaderSuite  extends FunSuite {
  import Pico._
  import PicoReader._

  test("pexpr parse") {
    println(parseAll(pexpr, "(define func [x y] [(+ x y) (- x y)] 100 100.0 \"string\")"))
    assert(parseAll(pexpr, "(define func [x y] [(+ x y) (- x y)] 100 100.0 \"string\" true)").get ==
             ParList(PSymbol("define"),
                     PSymbol("func"),
                     BracketList(PSymbol("x"), PSymbol("y")),
                     BracketList(
                       ParList(PSymbol("+"), PSymbol("x"), PSymbol("y")),
                       ParList(PSymbol("-"), PSymbol("x"), PSymbol("y"))),
                     PInt(100), 
                     PFloat("100.".toFloat),
                     PString("string"),
                     PTrue 
             ))

  }
}
