package hexasignal.pico

import org.scalatest.FunSuite

class PicoVMSuite extends FunSuite {
  import Pico._
  import PicoParser.parse
  import PicoVM._

  test("run test vm") {
    val ast = parse("(define x 1) (define y 2) [x y]").get
    val vm = new PicoVM()
    assert( vm.run(ast, Environment())._1  ==
             EntList(EntInt(1), EntInt(2) ))

  }
}
