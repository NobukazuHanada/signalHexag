package hexasignal.sound


import hexasignal.pico.Runner.EntBool
import hexasignal.pico.Runner.EntTrue
import hexasignal.pico.Runner.EntNumber
import hexasignal.pico.Runner.Entity
import hexasignal.pico.Runner.EntForeignValue
import hexasignal.IDGenerator


import de.sciss.synth._
import ugen._
import Ops._


import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext.Implicits.global

object Toy {
  sealed abstract class Toy
  sealed abstract class Env
  case class SinOsc(freq:Double, pharse:Double) extends Toy
  case class Perc(attack:Double, releaseTime:Double, level:Double, curve:Double) extends Env
  case class PercSin(s:SinOsc, p: Perc) extends Toy
}


object Sound {
  val cfg = Server.Config()
  val serverPromise : Promise[Server] = Promise[Server]()
  val server : Future[Server] = serverPromise.future
  import scala.collection.mutable.Map 
  val synthTable : Map[String,Synth] = Map()
  var loadSinOscCounter = 0

  cfg.program = "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/scsynth"

  Future {
    Server.boot(config=cfg) {
      case ServerConnection.Running(s) =>
        serverPromise success s
    }
  }

  def quit : Future[Boolean] = {
    server.map{ server =>
      server.freeAll()
      server.quit()
      true
    }
  }

  def playSin(freq: Int, phrase: Int) = {
    for(serv <- server){
      if( synthTable == Map() ){
        val synthDef = SynthDef("sinOsc") {
          val f =  "freq".kr(freq)
          val p = "phrase".kr(phrase)
          val amp = "amp" .kr(  1.0)
          Out.ar(0, Pan2.ar(SinOsc.ar(f,p) * amp))
        }
        val s = synthDef.play(serv)
        val id = IDGenerator.generate.IdString
        synthTable(id) = s
      }
      for((id, synth) <- synthTable){
        synth.set("freq" -> freq)
        synth.set("phrase" -> phrase)
        synth.set("amp" -> phrase)
      }
    }
  }


  def play : PartialFunction[Seq[Entity], EntBool] = {
    case Seq(EntForeignValue(Toy.SinOsc(freq, phrase)))  =>
      for(serv <- server){
        if( synthTable == Map() ){
          val synthDef = SynthDef("sinOsc") {
            val f =  "freq".kr(freq)
            val p = "phrase".kr(phrase)
            val amp = "amp" .kr(  1.0)
            Out.ar(0, Pan2.ar(SinOsc.ar(f,p) * amp))
          }
          val s = synthDef.play(serv)
          val id = IDGenerator.generate.IdString
          synthTable(id) = s
        }
        for((id, synth) <- synthTable){ 
          synth.set("freq" -> freq)
          synth.set("phrase" -> phrase)
          synth.set("amp" -> phrase)
        }
      }
      EntTrue
    case Seq(EntForeignValue(Toy.PercSin(Toy.SinOsc(freq, phrase),
                                         Toy.Perc(a,r,c,l))))  =>

      val synthDef = SynthDef("perc") {
        val f =  "freq".kr(freq)
        val p = "phrase".kr(phrase)
        val amp = "amp" .kr(  1.0)
        val env = EnvGen.ar(Env.perc(a,r)) 
        Out.ar(0, Pan2.ar(SinOsc.ar(f,p) * env))
      }
      EntTrue
  }

  import hexasignal.pico.Runner._
  import hexasignal.pico.Pico._

  def sinOsc : PartialFunction[Seq[Entity], Toy.SinOsc] = {
    case Seq(EntNumber(freq), EntNumber(phrase)) =>
      Toy.SinOsc(freq, phrase)

  }

  def perc : PartialFunction[Seq[Entity],  Toy.Perc] = {
    case Seq(EntNumber(attack), EntNumber(releaseTime), EntNumber(level), EntNumber(curve)) =>
      Toy.Perc(attack, releaseTime, level, curve)
  }

  def mul : PartialFunction[Seq[Entity], Toy.PercSin] = {
    case Seq(EntForeignValue(s:Toy.SinOsc), EntForeignValue(p:Toy.Perc)) =>
      Toy.PercSin(s,p)
  }

}
