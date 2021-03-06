package hexasignal.natsuki

import scala.util.parsing.combinator._
import scala.util.parsing.input.{
  NoPosition,
  OffsetPosition,
  Reader,
  CharSequenceReader
}

class Parser extends RegexParsers{
  override val skipWhitespace = false

  val w = whiteSpace
  var updateScprTable = true
  var positionTable = PositionTable()
  var sexprTable = SExprTable()

  def setTable[T <: SExpr](t:T) : T = {
    if( updateScprTable ){ sexprTable = sexprTable + t }
    t
  }


  def ranged[T](p: => Parser[T]) : Parser[(Range,T)] = Parser { in =>
    p(in) match {
      case Success(i:T, in1) =>
        Success({
                  val (posRef, newPositionTable) =
                    positionTable.createPos(in.pos.asInstanceOf[OffsetPosition].offset)

                  val (posRef2, newPositionTable1) =
                    newPositionTable.createPos(in1.pos.asInstanceOf[OffsetPosition].offset)
                  positionTable = newPositionTable1
                  (Range(posRef, posRef2), i)}, in1)
      case ns: NoSuccess => ns
    }
   }

  def t : Parser[Atom.Bool] =
      ranged("true".r) ^^ { case (i,_) =>
        setTable(Atom.Bool(true,i))
      }


  def f : Parser[Atom.Bool] =
    ranged("false".r) ^^ { case (i, t) =>
      setTable(Atom.Bool(false, i))
    }
    

  def bool : Parser[Atom.Bool] =
    t | f

  def symbol : Parser[Atom.Sym] =
    ranged("""[a-z!?%#$&*+\-\\*.<=>~@_][[a-zA-Z!?#%$&*+\-\\*.<=>~@_]]*""".r) ^^ { case (i, m) =>
        val s = m.toString
      setTable(Atom.Sym(s, i))
    }

  def v : Parser[Atom.Var] =
    ranged("""[A-Z]\w*""".r) ^^ { case (i,m) =>
      val s = m.toString()
      setTable(Atom.Var(s, i))
    }

  def str : Parser[Atom.Str] =
    ranged(
      "\"" ~> """[^\"]*""".r <~ "\"") ^^ { case (i,m) =>
        val s = m.toString()
        setTable(Atom.Str(s, i))
      }

  def numI : Parser[Atom.Num.I] =
    ranged("""-?\d+""".r) ^^ { case (p,m) =>
      val i = m.toInt
      setTable(Atom.Num.I(i, p))
    }
    

  def numD : Parser[Atom.Num.D] =
    ranged(
      """-?\d+\.(\d)*""".r) ^^ { case (i,m) =>
        val d = m.toDouble
        setTable(Atom.Num.D(d, i))
      }

  def numR : Parser[Atom.Num.R] =
    ranged((numI <~ "/") ~ numI) ^^ {
      case (i, n ~ d) =>
        setTable(Atom.Num.R(n.i, d.i, i))
    }

  def metaVar : Parser[MetaVar] =
    ranged(
      "<" ~> """[^>]*""".r <~">"
    ) ^^ { case (i,v) =>
        setTable(MetaVar(v,i))
    }

  def atom : Parser[Atom] =
     metaVar | bool | symbol | v | str | numR | numD | numI 



  def consList : Parser[ConsList] =
    ranged("[" ~ rep(w) ~> repsep(sexpr, whiteSpace)  <~ rep(w) ~ "]") ^^ {
      case (i, list) =>
        setTable(ConsList(list.map(_.ref),i))
    }

  def paren[T](p:Parser[T]) : Parser[T]  = "(" ~ rep(w) ~> p <~ rep(w) ~ ")" 


  def fn : Parser[Fn] =
    ranged(
      paren( 
        symbol ~ w ~ repsep(v, w)  ~ w ~ "->"
          ~ w ~ repsep(sexpr, w)
      )
    ) ^^ {
      case (i, sym~w1~vList~w2~arrow~w3~exprs) =>
        setTable(Fn(sym.ref,
                    vList.map(_.ref),
                    exprs.map(_.ref),
                    i))
    }

  def setVar : Parser[SetVar] =
    ranged(
      paren( "set" ~ w ~> v ~ w ~ sexpr )
    ) ^^ {
      case (i, v~w1~expr) =>
        setTable(SetVar(v.ref, expr.ref, i))
    }

  def ifexpr : Parser[IF] =
    ranged(
      paren( "if" ~ w ~> 
        sexpr ~ w ~ sexpr ~ w ~ sexpr )
    ) ^^ { case (i, cond~w1~thenExpr~w2~elseExpr) =>
        setTable( IF(cond.ref,
                     thenExpr.ref,
                     Some(elseExpr.ref),
                     i))
          } | ranged(
    "(" ~ "if" ~ w ~>
      sexpr ~ w ~ sexpr <~ ")"
  ) ^^ { case (i, cond~w1~thenExpr) =>
        setTable(IF(cond.ref,
           thenExpr.ref,
           None,
           i))
        }

  def let : Parser[Let] =
    ranged(
      paren(( "let" ~ w ~ "[" ~> repsep(v~w~sexpr,w) <~ ("]" ~ w)) ~
        repsep(sexpr, w))
    ) ^^ { case (i, binding~exprs) =>
        
        val map = Map(binding.map { case v~w1~sexpr => (v.ref -> sexpr.ref)}:_*)
        setTable( Let(map,
            exprs.map(_.ref),
            i))
    }

  def apply : Parser[Apply] =
    ranged(
      paren( sexpr ~ w ~ repsep(sexpr, w) )
    ) ^^ {
      case (i, f~w~args) =>
        setTable(Apply(f.ref,
              args.map(sexpr => sexpr.ref),
              i))
    }


  def sexpr : Parser[SExpr] =
    fn | setVar | ifexpr | let | apply | consList | atom

  def sexprs : Parser[List[SExpr]] = repsep(sexpr, w)
}

object UnParser {
  import Atom._
  import Num._

  def unParse(sexpr:SExpr, sexprTable: SExprTable) : String = {
    sexpr match {
      case Sym(s,_,_) => s
      case Str(s,_,_) => "\"" + s + "\""
      case Var(s,_,_) => s
      case I(i,_,_) => i.toString()
      case D(d,_,_) => d.toString()
      case R(n,d,_,_) => s"$n/$d"
      case ConsList(list,_,_) =>
        list.map(unParse(_, sexprTable)).mkString("("," ",")")
      case Apply(func, args,_,_) =>
        val argsText = args.map(unParse(_, sexprTable)).mkString(" ")
        s"(${unParse(func,sexprTable)} $argsText)"
      case Fn(name, args, exprs, _, _) =>
        val argsText = args.map(unParse(_, sexprTable)).mkString(" ")
        val exprsText = exprs.map(unParse(_, sexprTable)).mkString(" ")
        s"(fn ${unParse(name, sexprTable)} $argsText -> $exprsText )"
      case SetVar(name, expr,_,_) =>
        s"(set ${unParse(name, sexprTable)}  ${unParse(expr, sexprTable)})"
      case IF(cond, t, e, _, _) =>
        val condText = unParse(cond, sexprTable)
        val thenText = unParse(t, sexprTable)
        val elsText = e match {
          case Some(e1) => unParse(e1, sexprTable)
          case None => ""
        }
        s"(if $condText $thenText $elsText)"
      case Let(bindings, expr, _, _) =>
        val bindingsText = (bindings.map { case (v,e) =>
                              val vText = unParse(v, sexprTable)
                              val eText = unParse(e, sexprTable)
                              s"$vText $eText"
                            }).mkString("["," ","]")
        val exprText = expr.map(unParse(_, sexprTable)).mkString(" ")
        s"(let $bindingsText $exprText)"
    }
  }

  def unParse(sexr:SExprRef[SExpr], sexprTable: SExprTable) : String = {
    unParse(sexprTable.map(sexr),
      sexprTable)
  }
}
