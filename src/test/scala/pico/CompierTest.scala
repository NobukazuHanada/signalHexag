package hexasignal.pico

import org.scalatest.FunSuite

class PicoCompilerSuite extends FunSuite {
  import Pico._
  import PicoParser._

  test("pico parser simple parse") {
    assert(parse("(define (add x y) (+ x y))").get ==
             PicoSentence(
               PicoDefine("add",
                          PicoLambda(PicoArgs(PicoSymbol("x"), PicoSymbol("y")),
                                     PicoApply(PicoSymbol("+"),
                                               PicoSymbol("x"),
                                               PicoSymbol("y"))))
             )
    )

  }

}

class PicoReaderSuite  extends FunSuite {
  import Pico._
  import PicoReader._

  test("pexpr parse") {
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
