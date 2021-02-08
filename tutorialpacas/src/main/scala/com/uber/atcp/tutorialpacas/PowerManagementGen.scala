package com.uber.atcp.tutorialpacas


import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._
import InputDistribution.Exponential

import scala.language.implicitConversions  
import scala.language.postfixOps



class StableLiftGen(nofEngines : Int) extends Component {
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
  }
}

class GenericSystem(
  nbOfEngines : Int, 
  wiring : (Vector[Engine], Vector[Sensor], Vector[Battery]) => Assertions
) extends Component {
  
  val nbOfMonitored = 4
  val nbOfFunctions = nbOfEngines / nbOfMonitored
  // components of the system
  val engines   = Subs(nbOfEngines)(Engine()) 
  val batteries = Subs(nbOfEngines)(Battery()) 
  val sensors   = Subs(nbOfEngines)(Sensor())
  val cpu       = Sub(CPU()) 
  val functions = Subs(nbOfFunctions)(TrimFunction())
  val observer  = Sub(StableLiftGen(nbOfEngines))

  assertions {
    // batteries to cpus mapping
    cpu.isPowered := batteries.map(_.power).reduce(_||_)

    // cpu to functions mapping
    functions.foreach(_.cpuIsRunning := cpu.isRunning)

    // batteries to engines
    // batteries to sensors
    wiring(engines, sensors, batteries)

    // engines to functions
    // sensors to function
    //   2 1  ||  3 4
    //   8 7  ||  5 6 
    for {
      i <- (0 until nbOfFunctions)
      j <- (0 until nbOfMonitored)
      k = i + j * nbOfFunctions
    } {
      engines(k).trim := functions(i).trimmed(j) 
      functions(i).thrustWorthys(j) := sensors(k).transmitted
    }

    // engines to sensors
    sensors.map(_.sensed) := engines.map(_.thrustWorthy)

    // engines to observer mapping
    observer.thrusts := engines.map(_.thrust)
  }
  val lightningStrike1 = Event(FailureRates.expm8)
  val lightningStrike2 = Event(FailureRates.expm8)
  transitions{
    Sync(lightningStrike1) With {batteries(0).fail.soft & batteries(2).fail.soft}
    Sync(lightningStrike2) With {batteries(1).fail.soft & batteries(3).fail.soft}
  }
}

def wiring(engines : Vector[Engine], sensors : Vector[Sensor], batteries : Vector[Battery]) : Assertions = {
  val nbOfEngines = engines.size
  val nbOfGroups = nbOfEngines / 4
  for {
    i <- (0 until nbOfGroups)
    j <- (0 until 4)
    k = i + j * nbOfGroups
  }{
    engines(k).isPowered := batteries(k).power || batteries((k + 2 * nbOfGroups) % nbOfEngines).power
    sensors(k).isPowered := batteries(k).power || batteries((k + 2 * nbOfGroups) % nbOfEngines).power
  }
}

def wiringSeg(engines : Vector[Engine], sensors : Vector[Sensor], batteries : Vector[Battery]) : Assertions = {
  val nbOfEngines = engines.size
  val nbOfGroups = nbOfEngines / 4
  for {
    i <- (0 until nbOfGroups)
    j <- (0 until 4)
    k = i + j * nbOfGroups
  }{
    engines(k).isPowered := batteries(k).power || batteries((k + 2 * nbOfGroups) % nbOfEngines).power
    sensors(k).isPowered := batteries(k).power || batteries((k + 2 * nbOfGroups) % nbOfEngines).power
  }
}

object PowerManagementGen extends App {

  val system = GenericSystem(4, wiring)
  val systemSeg = GenericSystem(8, wiringSeg)

  /* interactiveSimulationB(system) */

  for(model <- stochasticCheck(systemSeg)){

    val mcs = minimalCutSequencesBFS(model, systemSeg.observer.isOk, 7)

    /* println(mcs.size) */
  }
  
}