package com.nobkz.hexagonSingal.signalNetWork

import scala.collection.mutable.Set
import com.nobkz.hexagonSingal.signal.{Signaller, Signal}

class Node(val signaller : Signaller[Signal, Signal] ) {
  val from : Set[Node] = Set()
  val to : Set[Node] = Set()

  

}
