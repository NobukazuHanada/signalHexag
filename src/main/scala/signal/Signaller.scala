package hexasignal.signal;

import scala.collection.mutable.Set

trait Signaller[R <: Signal, S <: Signal ] {
  val connectFrom : Set[Signaller[S,Signal]] = Set()
  val connectTo : Set[Signaller[Signal,R]] = Set()
}
