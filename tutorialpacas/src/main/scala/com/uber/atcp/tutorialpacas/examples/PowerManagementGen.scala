package com.uber.atcp.tutorialpacas.examples


import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._

import scala.language.implicitConversions  
import scala.language.postfixOps

/** Models a power management system and trim system. */

/** Failure modes. */
enum FM derives Lifted {
  case Ok
  case Lost
}

given Ord[FM] {
  def lt(x : FM, y : FM) = 
    x == FM.Ok && y == FM.Lost
}

/** Things that can fail. */
trait CanFail { self: Component =>
  val state = State[FM](init = FM.Ok)
  val failure = Event()
  transitions {
    When (failure) If (state === FM.Ok) Then {state := FM.Lost}
  }
}

/** Things that are powered by external power. */
trait IsPowered {
  self: Component =>
  val powered = InFlow[Power]
}

/** Battery power. */
enum Power derives Lifted {
  case Absent
  case Present
}

/* Degradation order : Better is lower */
given Ord[Power] {
  def lt(x: Power, y: Power) = 
    x == Power.Present && y == Power.Absent
}

/** Power trim modes. */
enum Trim derives Lifted {
  /** Engine is suspended. */
  case Shutdown
  /** Engine operates in nominal mode. */
  case NoTrim
  /** Engine operates in over-powered mode. */
  case Over
}

/** Engine thrust */
enum Thrust derives Lifted {
  case NoThrust
  case Nominal
  case Over
}

/** Engine componentn model. */
class Engine extends Component with IsPowered {
  /** Intrinsic failure state. */
  val state = State[FM](init  = FM.Ok)
  /** Power trim mode. */
  val trim = InFlow[Trim]
  /** Resulting thrust. */
  val thrust = OutFlow[Thrust]
  /** Engine failure in nominal-power mode. */
  val failNoTrim = Event(InputDistribution.Exponential(1.0E-6))
  /** Engine failure in over-power mode. */
  val failOverTrim = Event(InputDistribution.Exponential(1.0E-5))
  transitions {
    When (failNoTrim)   If (state === FM.Ok && trim === Trim.NoTrim) Then { state := FM.Lost }
    When (failOverTrim) If (state === FM.Ok && trim === Trim.Over)   Then { state := FM.Lost }
  }
  assertions {
    thrust := If (state === FM.Ok && powered === Power.Present) Then {
      Switch(trim) {
        Trim.Over     -> Thrust.Over
        Trim.Shutdown -> Thrust.NoThrust
        Default(Thrust.Nominal)
      }
    } Else { 
      Thrust.NoThrust 
    }
  }
}

class Battery extends Component with CanFail {
  val power = OutFlow[Power]
  assertions {
    power := Switch(state) {
      FM.Lost -> Power.Absent
      Default(Power.Present) 
    }
  }
}

class CPU extends Component with CanFail with IsPowered {
  /** Failure mode sent to the functions mapped on the CPU. */
  val mode = OutFlow[FM]

  assertions {
    mode := If(powered === Power.Absent || state === FM.Lost) Then { FM.Lost } Else { FM.Ok }
  }
}

/** Manages the trimming of m monitored engines. */
class TrimFunction extends Component {
  // we assume:
  // - engines are disposed on a circle
  // - number of engines is a multiple of 4
  // - engine i has a mirror, left and right conjugate engines.
  //              (i)
  //               |
  // ((i+3n)%4) ---+--- ((i+n)%4)
  // conjugate     |    conjugate
  //          ((i+2n)%4) 
  //            mirror
  def conj1 (i: Int) = (i + 1) % 4
  def mirror(i: Int) = (i + 2) % 4
  def conj2 (i: Int) = (i + 3) % 4

  /** Diagnosed failure modes for engines */
  val monitored = InFlows[FM](4)
  /** Trim commands for engines */
  val trimmed = OutFlows[Trim](4)
  /** Failure state of the CPU supporting this function. */
  val cpuState = InFlow[FM]

  assertions {
    trimmed.zipWithIndex foreach { case (t,i) =>
      val c1 = monitored(conj1(i))
      val c2 = monitored(conj2(i))
      val mi = monitored(mirror(i))
      t := If(cpuState === FM.Lost) Then {
        Trim.NoTrim 
      } ElseIf(mi === FM.Lost) Then {
        Trim.Shutdown
      } ElseIf(c1 === FM.Lost || c2 === FM.Lost) Then {
        Trim.Over
      } Else {
        Trim.NoTrim
      }
    }
  }
}

/** Senses the current failure state of an engine. Can do false positives. */
class EngineSensor extends Component with CanFail with IsPowered {
  /** Sensed failure mode */
  val sensed = InFlow[FM]
  /** Transmitted failure mode */
  val transmitted = OutFlow[FM]
  // canProduceThrustTransmitted value stuck at lost if state is lost otherwise canProduceThrustSensed value is canProduceThrustTransmitted
  assertions {
    transmitted := If(state === FM.Lost || powered === Power.Absent) Then { FM.Lost } Else { sensed }
  }
}

type Batteries = Vector[Battery]
type IsPoweredz = Vector[IsPowered]
type Wiring = (Batteries, IsPoweredz) => Assertions

def one2one(b : Batteries, e : IsPoweredz): Assertions =
  e map (_.powered) := b map (_.power) 

def one2all(b : Batteries, e : IsPoweredz): Assertions = e foreach {
  _.powered := (b map (_.power) reduce (_ min _))
}

/** 
* TODO Failure condition: Thrust is not symmetric or too many engines have no thrust.
*/
class Observer(nofEngines : Int) extends Component {
  val thrusts = InFlows[Thrust](nofEngines)
  val stableLiftTab = OutFlows[Boolean](nofEngines)
  val stableLift = OutFlow[Boolean]
  private val nofKo = thrusts.map( t => If(t === Thrust.NoThrust) Then 1 Else 0).reduce( _ + _ )
  private val nofTrimGroups = nofEngines/4

  def conj1(i : Int)  = (i + 1 * nofTrimGroups) % nofEngines
  def mirror(i : Int) = (i + 2 * nofTrimGroups) % nofEngines
  def conj2(i : Int)  = (i + 3 * nofTrimGroups) % nofEngines
  private val evenThrust = thrusts.zipWithIndex.map{case (t, i) => 
    If(t === Thrust.NoThrust) Then {
      thrusts(conj1(i)) === Thrust.Over &&
      thrusts(conj2(i)) === Thrust.Over &&
      thrusts(mirror(i)) === Thrust.NoThrust
    } ElseIf (t === Thrust.Over) Then {
      thrusts(conj1(i)) === Thrust.NoThrust &&
      thrusts(conj2(i)) === Thrust.NoThrust &&
      thrusts(mirror(i)) === Thrust.Over
    } Else {
      true
    }
  }
  val isOk = OutFlow[Boolean]

  assertions { 
    stableLift := evenThrust.reduce{_&&_}
    stableLiftTab := evenThrust.toVector
    isOk := nofKo <= 2 && stableLift
    // TODO uneven thrust
  }
}

trait System(
  val wiring : Wiring,
  val nofBatteries: Int,
  val nofEngines: Int,
  val nofCpus: Int,
) extends Component {
  require(nofEngines % 4 == 0, "number of engines must be multiple of 4")
  val nofFunctions = nofEngines/4
  val nofMonitored = 4

  // components of the system
  val engines   = Subs(nofEngines)(Engine()) 
  val batteries = Subs(nofBatteries)(Battery()) 
  val cpus      = Subs(nofCpus)(CPU()) 
  val functions = Subs(nofFunctions)(TrimFunction())
  val observer = Sub(Observer(nofEngines))

  // computes which batteries are powering each cpu
  val cpu2batts = 
    (0 until nofBatteries)
    .map(i => (i, i % nofCpus))
    .groupBy(_._2)
    .view
    .mapValues(_.map(_._1))

  assertions {
    // batteries to cpus mapping
    cpus.zipWithIndex foreach { 
      case (cpu, i) => 
        cpu.powered := cpu2batts(i).map(j => batteries(j).power).reduce(_ min _)
    }

    // cpu to functions mapping
    functions.zipWithIndex foreach { 
      case (f, i) => f.cpuState := cpus(i % nofCpus).state 
    }

    // batteries to engines mapping
    wiring(batteries, engines)

    // engines to functions
    for {
      i <- (0 until nofFunctions)
      j <- (0 until nofMonitored)
      k = i + j * nofFunctions
    } {
      engines(k).trim := functions(i).trimmed(j) 
    }

    // engines to observer mapping
    engines.zip(observer.thrusts) foreach { case (e, t) =>  t := e.thrust }
  }
}