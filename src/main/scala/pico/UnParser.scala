package hexasignal.pico

object UnParser {
  import Pico._

  val indent = "    "

  def indenting(level:Int) : String =
    "\n" + ((new Array[String](level)) map ( _ => indent ) mkString "")

  def unparse(picoSentence: PicoSentence) : String =
    picoSentence match {
      case PicoSentence(exprs @ _*) =>
        exprs.foldRight(List[String]()) {
          (expr, texts) =>
          texts :+ unparseExpr(expr, 1)
        } mkString "\n\n"
    }

  def unparseExpr(picoExpr: PicoExpr, indentLevel : Int) : String =
    picoExpr match {
      case PicoInt(n) => n.toString()
      case PicoFloat(f) => f.toString()
      case PicoString(s) => "\"" + s + "\""
      case PicoSymbol(s) => s
      case PicoTrue => "true"
      case PicoFalse => "false"
      case PicoList(list @ _*) =>
        list map { unparseExpr(_, indentLevel) } mkString("[",",","]")
      case PicoLambda(PicoArgs(args @ _*), expr) =>
        val argsText = args map { case PicoSymbol(name) => name } mkString("("," ",")")
        s"(lambda $argsText ${unparseExpr(expr, indentLevel)})"
      case PicoDefine(name:String, expr:PicoExpr) =>
        s"(define $name${indenting(indentLevel)}${unparseExpr(expr, indentLevel+1)})"
      case PicoDefineLambda(name, PicoArgs(args @ _*) ,expr) =>
        val argsText = args map { case PicoSymbol(name) => name } mkString " "
        s"(define ($name $argsText)${indenting(indentLevel)}${unparseExpr(expr, indentLevel + 1)})"
      case PicoIf(cond, thn, els) =>
        els match {
          case Some(els) => s"(if $cond\n $thn\n $els)"
          case None => s"(if $cond\n $thn)"
        }
      case PicoLet(bindingMap, expr @ _*) =>
        val bindingMapText = bindingMap match {
          case BindingMap(map) =>
            val texts : Seq[String] =
              for((PicoSymbol(name), expr) <- map.toList)
              yield name + " " + unparseExpr(expr, indentLevel)
            texts.reverse mkString("[", indenting(indentLevel - 1) + "      ","]")
        }
        val exprTexts = expr map{ unparseExpr(_, indentLevel+1) }
        val exprText = exprTexts mkString(indenting(indentLevel), indenting(indentLevel), "")
        s"(let $bindingMapText${exprText})"
      case PicoApply(expr1, expr2 @ _*) =>
        val expr1Text = unparseExpr(expr1, indentLevel)
        val expr2Text = expr2 map(unparseExpr(_, indentLevel)) mkString(" ")
        s"($expr1Text $expr2Text)"
    }
}
