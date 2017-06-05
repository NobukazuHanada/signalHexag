package hexasignal.pico


object Environment {
  import Pico._
  import Runner._
  
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


class Environment private (val variableMap: Map[Pico.PicoSymbol, Runner.Entity]) {
  import Pico._
  import Runner._

  def get(symbol:PicoSymbol) : Option[Entity] =
    variableMap.get(symbol)

  def +(cell:(PicoSymbol, Entity)) : Environment =
    Environment(variableMap + cell)

  def addForeignFunc[F](name:String)(f:Seq[Entity] => F): Environment =
    this + (PicoSymbol(name) -> EntForeignFunc(f))
}
