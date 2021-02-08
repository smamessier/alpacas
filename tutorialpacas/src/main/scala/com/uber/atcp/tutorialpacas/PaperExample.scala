
import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._
  
import java.lang.System

import scala.language.implicitConversions

enum Failure derives Lifted {
  case Ok
  case Fail
}

import Failure._

given Ord[Failure] {
  def lt(x: Failure, y: Failure): Boolean = x == Ok && y == Fail
}

import InputDistribution._

class Engine extends Component {
  val thrust  = OutFlow[Failure]
  val power   = InFlow[Failure]
  val state   = State[Failure](init = Ok)
  val failure = Event(Exponential(1E-5))
  val repair  = Event(Dirac(1))
  assertions {
    thrust := If (power === Ok && state === Ok) Then Ok Else Fail
  }
  transitions {
    When (failure) If state === Ok   Then { state := Fail }
    When (repair)  If state === Fail Then { state := Ok }
  }
}

class Battery extends Component {
  val power   = OutFlow[Failure]
  val state   = State[Failure](init = Ok)
  val failure = Event(Exponential(1E-5))
  val repair  = Event(Dirac(5), 1.0)
  assertions { power := state }
  transitions {
    When (failure) If state === Ok   Then { state := Fail }
    When (repair)  If state === Fail Then { state := Ok }
  }
}

type Batteries = Vector[Battery]
type Engines   = Vector[Engine]
type Wiring    = (Batteries, Engines) => Assertions

class Powertrain(wiring: Wiring, n: Int) extends Component {
  val batteries = Subs(n)(Battery()) 
  val engines   = Subs(n)(Engine())
  val observer  = OutFlow[Boolean]
  val ccf       = Event(Exponential(1E-7))
  assertions {
    wiring(batteries, engines)
    observer := engines.map(_.thrust === Ok).reduce(_&&_)
  }
  transitions {
    Sync(ccf) With { batteries.map(_.failure.hard).reduce(_&_) }
  }
}

def one2one(b: Batteries, e: Engines): Assertions =
  e map (_.power) := b map (_.power)


def one2all(b: Batteries, e: Engines): Assertions =
  for (eng <- e) {
    eng.power := (b map (_.power) reduce (_ min _))
  }

val powertain121    = Powertrain(one2one, 2)
val powertrain12all = Powertrain(one2all, 2)

def time[T](c: => T): (T, Long) = {
  val begin = System.currentTimeMillis()
  val res = c
  val end = System.currentTimeMillis()
  (res, end - begin)
}

object PaperExample extends App {
  val (model, modeltime) = time(stochasticCheck(powertrain12all))
  println(s"Flattening time : $modeltime")
  for {
    model <- model
    (mcs, timeMCS) = time(minimalCutSetsBFS(model, powertrain12all.observer, 5))
    mcs <- mcs
  }{
    val (bdd, timeBDD) = time(structureFctBDD(model, mcs))
    mcs.foreach(println(_))
    println(s"MCS time: $timeMCS")
    println(s"BDD time: $timeBDD")
    for {
      t <- 1E2::1E1::1E2::1E3::1E4::1E5::Nil // first two runs are dry-runs added to let the JVM JIT our model
    }{
      println(s"\nDuration: $t")
      println(time(unreliabilityBDD(model, bdd, t, mcs)))// TODO time different parts of this (factor out bdd computation)
      println(time(unreliability(model, powertrain12all.observer, t, 100000, 8))) 
    }
  }
}
