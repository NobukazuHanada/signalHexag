package hexasignal.natsuki


import org.scalatest.FunSuite

class ReaderTest extends FunSuite {
  val reader = new Reader()
  import reader._
  import Atom._

  test("currect position of nastsuki reader result") { 
    val sourceText = "[1 \"a\" (+ 1 2) X_list text]"
    val DataList(resultDataList) = parseAll(sexpr, sourceText).get

    assert(resultDataList(0).getInputText(sourceText).get == "1")
    assert(resultDataList(1).getInputText(sourceText).get == "\"a\"")
    assert(resultDataList(2).getInputText(sourceText).get == "(+ 1 2)")
    assert(resultDataList(3).getInputText(sourceText).get == "X_list")
    assert(resultDataList(4).getInputText(sourceText).get == "text")
  }
}
 
