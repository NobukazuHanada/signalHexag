package hexasignal.signalNetWork

import scala.collection.mutable.Set
import hexasignal.signal.{Signaller, Signal}

class Node(val signaller : Signaller[Signal, Signal] ) {
  val from : Set[Node] = Set()
  val to : Set[Node] = Set()

  

}
