package hexasignal;

import scala.collection.mutable.Set
import hexasignal.signalNetWork.Node


object SignalNetWorking{
  def apply(name: String) : SignalNetWorking = new SignalNetWorking(name)
}

class SignalNetWorking private (val netWorkName : String ) {
  val nodes : Set[Node] = Set()

  def add(node:Node): Boolean = nodes.add(node)


}
