package com.nobkz.hexagonSingal.model


trait Watcher{
  def noticed()
}

trait Model {

  import scala.collection.mutable.ListBuffer

  val watchers : ListBuffer[Watcher] = ListBuffer()

  def addWatcher( watcher : Watcher )  {
    watchers += watcher
  }

  def notice() : Unit = {
    for( watcher <- watchers ) watcher.noticed()
  }
}
