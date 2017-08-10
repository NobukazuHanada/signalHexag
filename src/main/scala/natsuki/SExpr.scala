package hexasignal.natsuki

import scalafx.beans.property.{
  StringProperty,
  BooleanProperty,
  IntegerProperty,
  DoubleProperty,
  ObjectProperty
}

import scala.util.parsing.input.{
  Position,
  NoPosition,
  OffsetPosition
}

import scala.collection.mutable.MapBuilder

import scala.collection.immutable.{
  MapLike,
  Map,
  HashMap,
  AbstractMap
}

import scala.collection.generic.{
  CanBuildFrom,
  ImmutableMapFactory
}

import hexasignal.{
  Id,
  IDGenerator
}


object PositionRef {
  def generate : PositionRef =
    PositionRef(IDGenerator.generate("position ref"))
}
case class PositionRef(id: Id)

case class Position(pos : Int, ref : PositionRef){
  def +(i:Int) : Position =
    Position(i + pos, ref)
}

object PositionTable {
  def empty : PositionTable = new PositionTable(HashMap.empty)

  def apply(kv: (PositionRef, Position) *) =
    new PositionTable(HashMap(kv:_*))
}

case class PositionTable(map: HashMap[PositionRef, Position]){
  self =>

  def createPos(pos:Int) : (PositionRef, PositionTable) = {
    val position = Position(pos, PositionRef.generate)
    (position.ref, this + position)
  }

  def +(v: Position) : PositionTable =
   this + (v.ref -> v) 

  def +(kv:(PositionRef, Position)) =
    new PositionTable(this.map + kv)

  def ++(that: PositionTable) =
    new PositionTable(this.map ++ that.map)

  def -(key: PositionRef) : PositionTable =
    new PositionTable(map - key)

  def update(kv: (PositionRef, Position)) : PositionTable = {
   (map get kv._1) match {
     case Some(oldPos) =>
       val diff = kv._2.pos - oldPos.pos
       val newMap : HashMap[PositionRef, Position] = (map - kv._1).map(
         { case (ref, position) =>
           if( position.pos > oldPos.pos ){
             (ref, position + diff)
           }else{
             (ref, position)
           }
         })
       new PositionTable(newMap + kv)
     case None =>
       this + kv
   }
  }

  override def toString: String =
    map.toList.map({case (a,b) => s"$a -> $b" } ).mkString("(", ",",")")
}


object SExprTable {
  def empty : SExprTable = new SExprTable(HashMap.empty)

  def apply(kv: (SExprRef[SExpr], SExpr) *) =
    new SExprTable(HashMap(kv:_*))
}

case class SExprTable(val map:
                     HashMap[SExprRef[SExpr],SExpr]) {
  def +(s: SExpr): SExprTable =
    new SExprTable(map + (s.ref -> s))

  def -(ref: SExprRef[SExpr]): SExprTable =
    new SExprTable(map - ref)

  def update[S <: SExpr](ref: SExprRef[S], newSexpr:S) : SExprTable =
    map get ref match {
      case Some(sexpr) =>
        val oldRemoveSExprTable = this - ref
        oldRemoveSExprTable + newSexpr
      case None =>
        this + newSexpr
    }

  def search(pos: Int , positionTable: PositionTable) : Option[SExpr] = {
    var startPos = 0
    var findExpr : Option[SExpr] = None
    for( (ref, expr) <- map ){
      val p = expr.pos
      val sPos  = positionTable.map(p.start).pos
      val ePos  = positionTable.map(p.end).pos
      if( startPos < sPos &&
           sPos < pos &&
           pos < ePos
      ){
        startPos = sPos
        findExpr = Some(expr)
      }
    }
    findExpr
  }

  def isEmpty : Boolean =
    map.isEmpty

  def lastPos(positionTable: PositionTable) : Int =
    (for {
      (_, sexpr) <- map
      pos = sexpr.pos
      endPos = positionTable.map(pos.end).pos
    } yield endPos).max
}

object SExprRef{
  def generate[S <: SExpr]  : SExprRef[S] =
    SExprRef[S](IDGenerator.generate("sexpr ref"))
}
case class SExprRef[+S <: SExpr](id: Id)

case class Range(start: PositionRef, end: PositionRef) 

trait Ranged{
  val pos : Range
}


sealed abstract class SExpr extends Ranged {
  val ref : SExprRef[SExpr]
  def updateRef(ref: SExprRef[SExpr]) : SExpr 
}

case class Apply(func: SExprRef[SExpr],
                 args: List[SExprRef[SExpr]],
                 pos: Range,
                 val _ref: Option[SExprRef[Apply]] = None
) extends SExpr {
  override val ref : SExprRef[Apply] = _ref match {
    case Some(r) => r
    case None => SExprRef.generate[Apply]
  }
  def updateRef(ref: SExprRef[SExpr]) : SExpr = 
    Apply(func, args, pos, Some(ref.asInstanceOf[SExprRef[Apply]]))

}

case class Let(varMap: Map[SExprRef[Atom.Var], SExprRef[SExpr]],
               expr: List[SExprRef[SExpr]],
               pos: Range,
               val _ref: Option[SExprRef[Let]] = None
) extends SExpr {
  override val ref : SExprRef[Let] = _ref match {
    case Some(r) => r
    case None => SExprRef.generate[Let]
  }
  def updateRef(ref: SExprRef[SExpr]) : SExpr =
    Let(varMap, expr, pos, Some(ref.asInstanceOf[SExprRef[Let]]))
}

case class IF(cond: SExprRef[SExpr],
              thenExpr: SExprRef[SExpr],
              elseExpr: Option[SExprRef[SExpr]],
              pos: Range,
              val _ref: Option[SExprRef[IF]] = None
) extends SExpr {
  override val ref : SExprRef[IF] = _ref match {
    case Some(r) => r
    case None => SExprRef.generate[IF]
  }
  def updateRef(ref: SExprRef[SExpr]) : SExpr =
    IF(cond, thenExpr, elseExpr, pos, Some(ref.asInstanceOf[SExprRef[IF]]))
}

case class SetVar(name: SExprRef[Atom.Var],
                  expr: SExprRef[SExpr],
                  pos: Range,
                  val _ref: Option[SExprRef[SetVar]] = None
) extends SExpr {
  override val ref : SExprRef[SetVar] = _ref match {
    case Some(r) => r
    case None => SExprRef.generate[SetVar]
  }
  def updateRef(ref: SExprRef[SExpr]) : SExpr =
    SetVar(name, expr, pos, Some(ref.asInstanceOf[SExprRef[SetVar]]))
}

case class Fn(name: SExprRef[Atom.Sym],
              args: List[SExprRef[Atom.Var]],
              exprs: List[SExprRef[SExpr]],
              pos: Range,
              val _ref: Option[SExprRef[Fn]] = None
) extends SExpr {
  override val ref : SExprRef[Fn] = _ref match {
    case Some(r) => r
    case None => SExprRef.generate[Fn]
  }
  def updateRef(ref: SExprRef[SExpr]) : SExpr =
    Fn(name, args, exprs, pos, Some(ref.asInstanceOf[SExprRef[Fn]]))
}

case class ConsList(list: List[SExprRef[SExpr]],
                    pos: Range,
                    val _ref: Option[SExprRef[ConsList]] = None
) extends SExpr {
  override val ref : SExprRef[ConsList] = _ref match {
    case Some(r) => r
    case None => SExprRef.generate[ConsList]
  }

  def updateRef(ref: SExprRef[SExpr]) : SExpr =
    ConsList(list, pos, Some(ref.asInstanceOf[SExprRef[ConsList]]))
}

sealed abstract class Atom extends SExpr

object Atom {
  case class Sym(n: String, pos: Range, val _ref: Option[SExprRef[Sym]] = None) extends Atom {
    override val ref : SExprRef[Sym] = _ref match {
      case Some(r) => r
      case None => SExprRef.generate[Sym]
    }

    def update(newString: String) : Sym =
      Sym(newString, pos, Some(ref))

    def updateRef(ref: SExprRef[SExpr]) : SExpr =
      Sym(n, pos, Some(ref.asInstanceOf[SExprRef[Sym]]))
  }
  case class Var(n: String, pos: Range, val _ref: Option[SExprRef[Var]] = None)  extends Atom  {
    override val ref : SExprRef[Var] = _ref match {
      case Some(r) => r
      case None => SExprRef.generate[Var]
    }

    def update(newString: String) : Var =
      Var(newString, pos, Some(ref))

    def updateRef(ref: SExprRef[SExpr]) : SExpr =
      Var(n, pos, Some(ref.asInstanceOf[SExprRef[Var]]))
  }
  case class Str(n: String, pos: Range,
                 val _ref: Option[SExprRef[Str]] = None
  ) extends Atom {
    override val ref : SExprRef[Str] = _ref match {
      case Some(r) => r
      case None => SExprRef.generate[Str]
    }

    def update(newString: String) : Str =
      Str(newString, pos, Some(ref))

    def updateRef(ref: SExprRef[SExpr]) : SExpr =
      Str(n, pos, Some(ref.asInstanceOf[SExprRef[Str]]))
  }
  abstract class Num extends Atom 
  object Num {
    case class I(i:Int, pos: Range, val _ref: Option[SExprRef[I]] = None) extends Num {
      override val ref : SExprRef[I] = _ref match {
        case Some(r) => r
        case None => SExprRef.generate[I]
      }

      def updateRef(ref: SExprRef[SExpr]) : SExpr =
        I(i, pos, Some(ref.asInstanceOf[SExprRef[I]]))
    }
    case class D(d:Double,
                 pos: Range,
                 val _ref: Option[SExprRef[D]] = None
    ) extends Num {
      override val ref : SExprRef[D] = _ref match {
        case Some(r) => r
        case None => SExprRef.generate[D]
      }
      
      def updateRef(ref: SExprRef[SExpr]) : SExpr =
        D(d, pos, Some(ref.asInstanceOf[SExprRef[D]]))
    }
    case class R(n:Int,d:Int,
                 pos: Range,
                 val _ref: Option[SExprRef[R]] = None
    ) extends Num {
      override val ref : SExprRef[R] = _ref match {
        case Some(r) => r
        case None => SExprRef.generate[R]
      }

      def updateRef(ref: SExprRef[SExpr]) : SExpr =
        R(n, d, pos, Some(ref.asInstanceOf[SExprRef[R]]))
    }
  }
  case class Bool(b:Boolean, pos: Range,
                  val _ref : Option[SExprRef[Bool]] = None) extends Atom { 
    override val ref : SExprRef[Bool] = _ref match {
      case Some(r) => r
      case None => SExprRef.generate[Bool]
    }

    def updateRef(ref: SExprRef[SExpr]) : SExpr =
      Bool(b, pos, Some(ref.asInstanceOf[SExprRef[Bool]]))
  }
}



abstract class FilterSExpr extends Atom

case class MetaVar(s: String, pos: Range,
                   val _ref : Option[SExprRef[MetaVar]] = None)
    extends FilterSExpr {
  override val ref : SExprRef[MetaVar] = _ref match {
    case Some(r) => r
    case None => SExprRef.generate[MetaVar]
  }
  def updateRef(r:SExprRef[SExpr]) : MetaVar =
    MetaVar(s,pos,Some(r.asInstanceOf[SExprRef[MetaVar]]))

}

