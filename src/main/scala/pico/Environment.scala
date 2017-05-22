package hexasignal.pico


object Environment {
  import Pico._
  import PicoVM._
  def apply() : Environment = new Environment(Map[PicoSymbol, Entity]())

  def apply(maps:(PicoSymbol, Entity)*) : Environment = {
    val mutableMap = scala.collection.mutable.Map[PicoSymbol, Entity]()
    for(elem <- maps) {
      mutableMap += elem
    }
    new Environment(mutableMap.toMap)
  }

  def apply(map:Map[PicoSymbol, Entity]) = new Environment(map)
}


class Environment private (val varialbeMap: Map[Pico.PicoSymbol, PicoVM.Entity]) {
  import Pico._
  import PicoVM._

  def get(symbol:PicoSymbol) : Option[Entity] =
    varialbeMap.get(symbol)

  def +(cell:(PicoSymbol, Entity)) : Environment =
    Environment(varialbeMap + cell)
}
