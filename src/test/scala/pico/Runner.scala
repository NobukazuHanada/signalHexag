package hexasignal.pico

import org.scalatest.FunSuite

class RunnerVMSuite extends FunSuite {
  import Pico._
  import PicoParser.parse
  import Runner._
  import Result._

  test("run test vm") {
    val Success(ast) = parse("(define x 1) (define y 2) [x y]")
    implicit val environment = Environment()
    assert( Runner.run(ast).map(_._1)  ==
             EntList(EntInt(1), EntInt(2)).toResult
    )
  }

  test("external value") {
    val Success(ast) = parse("(define rect1 (rectangle 1 2 3 4)) rect1")
    case class Rect(x:Int, y:Int, w:Int, h:Int)
    implicit val environment =
      Environment().addForeignFunc("rectangle") {
        case Seq(EntInt(x), EntInt(y), EntInt(w), EntInt(h)) =>
          Rect(x,y,w,h)
      }
    assert(Runner.run(ast).map(_._1) == EntForeignValue(Rect(1,2,3,4)).toResult)
  }

  test("use define") {
    val input =
      """(define x
 1)"""
    val Success(ast) = parse(input)
    implicit val environment = Environment()
    val Success((result, env)) = Runner.run(ast)
    assert(result == EntInt(1))
    assert(env.variableMap ==
             Map(PicoSymbol("x") -> EntInt(1))
    )
  }

  test("use lambda") {
    val input = """(define func (lambda (a b) [a b]))

(define result (func 1 2))

"""

    val Success(ast) = parse(input)
    implicit val environment = Environment()
    val Success((result, env)) = Runner.run(ast)

    assert(result == EntList(EntInt(1), EntInt(2)))
  }

}
