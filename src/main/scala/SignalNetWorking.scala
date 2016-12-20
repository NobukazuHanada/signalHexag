package com.nobkz.hexagonSingal;

import scala.collection.mutable.Set
import com.nobkz.hexagonSingal.signalNetWork.Node


object SignalNetWorking{
  def apply(name: String) : SignalNetWorking = new SignalNetWorking(name)
}

class SignalNetWorking private (val netWorkName : String ) {
  val nodes : Set[Node] = Set()

  def add(node:Node): Boolean = nodes.add(node)


}
