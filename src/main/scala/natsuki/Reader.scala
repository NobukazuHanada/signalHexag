package hexasignal.natsuki

import scala.util.parsing.combinator._
import scala.util.parsing.input.{
  NoPosition,
  OffsetPosition
}


class Reader extends RegexParsers{
  override val skipWhitespace = false

   def ranged[T <: Range](p: => Parser[T]) : Parser[T] = Parser { in =>
    p(in) match {
      case Success(t, in1) => Success(
        if(t.pos == NoPosition){
          t setPos in.pos
          t setEndPos in1.pos
        }else t, in1)
      case ns: NoSuccess => ns
    }
   }

  def t : Parser[Atom.Bool] =
    ranged(
      "true".r ^^ { _ =>
        Atom.Bool.True
      }
    )


  def f : Parser[Atom.Bool] =
    ranged(
      "false".r ^^ { _ =>
        Atom.Bool.False
      }
    )

  def bool : Parser[Atom.Bool] =
    t | f

  def symbol : Parser[Atom.Sym] =
    ranged(
      """[a-z!?#%$&*+\-\\*.<=>~@_][[a-zA-Z!?#%$&*+\-\\*.<=>~@_]]*""".r ^^ { m =>
        val s = m.toString
        Atom.Sym(s)
      }
    )

  def v : Parser[Atom.Var] =
    ranged(
    """[A-Z]\w*""".r ^^ { m =>
      val s = m.toString()
      Atom.Var(s)
    }
    )


  def str : Parser[Atom.Str] =
    ranged(
      "\"" ~ """[^\"]*""".r ~ "\"" ^^ { m =>
        val s = m.toString()
        Atom.Str(s)
      }
    )

  def numI : Parser[Atom.Num.I] =
    ranged(
      """-?\d+""".r ^^ { m =>
        val i = m.toInt
        Atom.Num.I(i)
      }
    )

  def numD : Parser[Atom.Num.D] =
    ranged(
      """-?\d+\.(\d)*""".r ^^ { m =>
        val d = m.toDouble
        Atom.Num.D(d)
      }
    )

  def numR : Parser[Atom.Num.R] =
    ranged(
      (numI <~ "/") ~ numI ^^ {
        case n ~ d =>
          Atom.Num.R(n.i, d.i)
      }
    )

  def atom : Parser[Atom] =
    bool | symbol | v | str | numR | numD | numI

  def dataList : Parser[DataList] =
    ranged(
      "[" ~> repsep(sexpr, whiteSpace)  <~ "]" ^^ { DataList(_) }
    )

  def evalList : Parser[EvalList] =
    ranged(
      "(" ~> repsep(sexpr, whiteSpace)  <~ ")" ^^ { EvalList(_) }
    )

  def sexpr : Parser[SExpr] =
    dataList | evalList | atom

}
