package hexasignal.pico

import org.scalatest.FunSuite

class PicoVMSuite extends FunSuite {
  import PicoParser.parse
  import PicoVM._
  import Result._

  test("run test vm") {
    val Success(ast) = parse("(define x 1) (define y 2) [x y]")
    val vm = new PicoVM()
    assert( vm.run(ast).map(_._1)  ==
             EntList(EntInt(1), EntInt(2)).toResult
    )

  }

  test("external value") {
    val Success(ast) = parse("(define rect1 (rectangle 1 2 3 4)) rect1")
    val vm = new PicoVM()
    case class Rect(x:Int, y:Int, w:Int, h:Int)
    implicit val environment =
      Environment().addForeignFunc("rectangle") {
      case Seq(EntInt(x), EntInt(y), EntInt(w), EntInt(h)) =>
        Rect(x,y,w,h)
    }
    assert(vm.run(ast).map(_._1) == EntForeignValue(Rect(1,2,3,4)).toResult)
  }
}
