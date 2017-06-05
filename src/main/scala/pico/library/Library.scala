package hexasignal.pico.library

import hexasignal.pico.Runner
import Runner._

object Standard {

  val add : Seq[Entity] => EntValue =
    {
      case Seq(EntInt(i1), EntInt(i2)) => EntInt(i1 + i2)
      case Seq(EntFloat(f1), EntFloat(f2)) => EntFloat(f1 + f2)
      case Seq(EntString(s1), EntString(s2)) => EntString(s1 + s2)
      case Seq(first, second, rest @ _*) =>
       add(add(Seq(first, second)) +: rest) 
    }

  val sub : Seq[Entity] => EntValue =
  {
    case Seq(EntInt(i1), EntInt(i2)) => EntInt(i1 - i2)
    case Seq(EntFloat(f1), EntFloat(f2)) => EntFloat(f1 - f2)
    case Seq(first, second, rest @ _*) =>
      sub(sub(Seq(first, second)) +: rest)
  }


  val mul : Seq[Entity] => EntValue =
  {
    case Seq(EntInt(i1), EntInt(i2)) => EntInt(i1 * i2)
    case Seq(EntFloat(f1), EntFloat(f2)) => EntFloat(f1 * f2)
    case Seq(first, second, rest @ _*) =>
      mul(mul(Seq(first, second)) +: rest)
  }


  val div : Seq[Entity] => EntValue =
  {
    case Seq(EntInt(i1), EntInt(i2)) => EntInt(i1 / i2)
    case Seq(EntFloat(f1), EntFloat(f2)) => EntFloat(f1 / f2)
    case Seq(first, second, rest @ _*) =>
      div(eq(Seq(first, second)) +: rest)
  }

  val eq : Seq[Entity] => EntValue =
  {

    case Seq(a, b) => if( a == b ) EntTrue else EntFalse
    case Seq(first, second, rest @ _*) =>
      if( first == second )
        eq(second +: rest)
      else
        EntFalse
  }

  val not : Seq[Entity] => EntValue = {
    case Seq(EntTrue) => EntFalse
    case Seq(EntFalse) => EntTrue
  }

}
