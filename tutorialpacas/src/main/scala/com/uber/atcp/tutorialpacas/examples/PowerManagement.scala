package com.uber.atcp.tutorialpacas.examples


import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._
import InputDistribution.Exponential

import scala.language.implicitConversions  
import scala.language.postfixOps


object PowerManagement extends App {

  /** Models a power management system and trim system. */
  object FailureRates {
    val expm6 = Exponential(1.0E-6)
    val expm5 = Exponential(1.0E-5)
    val expm4 = Exponential(1.0E-4)
    val expm3 = Exponential(1.0E-3)
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
  class Engine extends Component {

    /** Power trim mode. */
    val trim = InFlow[Trim]

    /** Produced thrust. */
    val thrust = OutFlow[Thrust]

    /** True iff the engine received power. */
    val isPowered = InFlow[Boolean]

    /** Intrinsic failure state. */
    val isFailed = State[Boolean](init = false)

    /** Engine failure in nominal-power mode. */
    val failNoTrim = Event(FailureRates.expm6)

    /** Engine failure in over-power mode. */
    val failOverTrim = Event(FailureRates.expm5)

    transitions {
      When (failNoTrim)   If (!isFailed && trim === Trim.NoTrim && isPowered) Then { isFailed := true }
      When (failOverTrim) If (!isFailed && trim === Trim.Over && isPowered)   Then { isFailed := true }
    }
    
    assertions {
      thrust := If (!isFailed && isPowered) Then {
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

  class Battery extends Component {

    /** True iff it produces power. */
    val power = OutFlow[Boolean]

    /** Intrinsic failure state. */
    val isFailed = State[Boolean](init = false)
    val fail = Event(FailureRates.expm4)

    transitions {
      When (fail) If (!isFailed) Then { isFailed := true }
    }

    assertions {
      power := !isFailed
    }
  }

  class CPU extends Component {
    
    /** True iff it receives power. */
    val isPowered = InFlow[Boolean]
    
    /** Failure mode sent to the functions mapped on the CPU. */
    val isRunning = OutFlow[Boolean]
    
    /** Intrisic failure state. */
    val isFailed = State[Boolean](init = false)
    val fail = Event(FailureRates.expm6)

    transitions {
      When (fail) If (!isFailed) Then { isFailed := true }
    }

    assertions {
      isRunning := isPowered && !isFailed
    }

  }

  /** Manages the trimming of m monitored engines. */
  class TrimFunction extends Component {
    // we assume:
    //              (0)
    //               |
    //        (3) ---+--- (1)
    //    conjugate  | conjugate
    //              (2) 
    //            mirror

    /** Diagnosed failure modes for engines */
    val monitored = InFlows[Boolean](4)
    /** Trim commands for engines */
    val trimmed = OutFlows[Trim](4)
    /** Failure state of the CPU supporting this function. */
    val cpuIsRunning = InFlow[Boolean]

    assertions {
      trimmed(0) := If(!cpuIsRunning) Then {
        Trim.NoTrim
      } ElseIf(monitored(2)) Then {
        Trim.Shutdown
      } ElseIf(monitored(1) || monitored(3)) Then {
        Trim.Over
      } Else {
        Trim.NoTrim
      }

      trimmed(1) := If(!cpuIsRunning) Then {
        Trim.NoTrim
      } ElseIf(monitored(3)) Then {
        Trim.Shutdown
      } ElseIf(monitored(2) || monitored(0)) Then {
        Trim.Over
      } Else {
        Trim.NoTrim
      }

      trimmed(2) := If(!cpuIsRunning) Then {
        Trim.NoTrim
      } ElseIf(monitored(0)) Then {
        Trim.Shutdown
      } ElseIf(monitored(1) || monitored(3)) Then {
        Trim.Over
      } Else {
        Trim.NoTrim
      }

      trimmed(3) := If(!cpuIsRunning) Then {
        Trim.NoTrim
      } ElseIf(monitored(1)) Then {
        Trim.Shutdown
      } ElseIf(monitored(0) || monitored(2)) Then {
        Trim.Over
      } Else {
        Trim.NoTrim
      }
    }
  }

  /** Senses the current failure state of an engine. Can do false positives. */
  class EngineSensor extends Component {
    /** Sensed failure mode */
    val sensed = InFlow[Boolean]

    /** Transmitted failure mode */
    val transmitted = OutFlow[Boolean]

    /** True iff it receives power. */
    val isPowered = InFlow[Boolean]

    /** Intrisic failure state. */
    val isFailed = State[Boolean](init = false)
    val fail = Event(FailureRates.expm6)

    transitions {
      When (fail) If (!isFailed) Then { isFailed := true }
    }

    // canProduceThrustTransmitted value stuck at false if failed otherwise canProduceThrustSensed value is canProduceThrustTransmitted
    assertions {
      transmitted := If(isFailed || !isPowered) Then { false } Else { sensed }
    }
  }

  type Batteries = Vector[Battery]


  class StableLift extends Component {
    val thrusts = InFlows[Thrust](4)
      
    val isOk = OutFlow[Boolean]

    assertions { 
      isOk :=(thrusts(0) === Thrust.Nominal &&
              thrusts(1) === Thrust.Nominal &&
              thrusts(2) === Thrust.Nominal &&
              thrusts(3) === Thrust.Nominal) || 
              (thrusts(0) === Thrust.Over &&
              thrusts(1) === Thrust.NoThrust &&
              thrusts(2) === Thrust.Over &&
              thrusts(3) === Thrust.NoThrust) ||
              (thrusts(0) === Thrust.NoThrust &&
              thrusts(1) === Thrust.Over &&
              thrusts(2) === Thrust.NoThrust &&
              thrusts(3) === Thrust.Over)
    }
  }

  class System extends Component {

    // components of the system
    val engines   = Subs(4)(Engine()) 
    val batteries = Subs(4)(Battery()) 
    val sensors   = Subs(4)(EngineSensor())
    val cpu       = Sub(CPU()) 
    val function  = Sub(TrimFunction())
    val observer  = Sub(StableLift())

    assertions {
      // batteries to cpus mapping
      cpu.isPowered := batteries(0).power || batteries(1).power || batteries(2).power || batteries(3).power

      // cpu to functions mapping
      function.cpuIsRunning := cpu.isRunning

      // batteries to engines mapping
      // engines to functions
      // engines to sensors
      // batteries to sensors
      // sensors to function
      // engines to observer mapping
      for (i <- 0 until 4){
        engines(i).isPowered  := batteries(i).power
        engines(i).trim       := function.trimmed(i) 
        sensors(i).sensed     := engines(i).isFailed
        sensors(i).isPowered  := batteries(i).power
        function.monitored(i) := sensors(i).transmitted
        observer.thrusts(i)   := engines(i).thrust
      }
    }
  }


  class SystemSeg extends Component {

    // components of the system
    val engines   = Subs(4)(Engine()) 
    val batteries = Subs(4)(Battery()) 
    val sensors   = Subs(4)(EngineSensor())
    val cpu       = Sub(CPU()) 
    val function  = Sub(TrimFunction())
    val observer  = Sub(StableLift())

    assertions {
      // batteries to cpus mapping
      cpu.isPowered := batteries(0).power || batteries(1).power || batteries(2).power || batteries(3).power

      // cpu to functions mapping
      function.cpuIsRunning := cpu.isRunning

      // batteries to engines mapping
      // engines to functions
      // engines to sensors
      // batteries to sensors
      // sensors to function
      // engines to observer mapping
      for (i <- 0 until 4){
        engines(i).isPowered  := batteries(i).power
        engines(i).trim       := function.trimmed(i) 
        sensors(i).sensed     := engines(i).isFailed
        sensors(i).isPowered  := batteries((i + 1) % 4).power
        function.monitored(i) := sensors(i).transmitted
        observer.thrusts(i)   := engines(i).thrust
      }
    }
  }
  val system = System()

  val systemSeg = SystemSeg()

  for(model <- stochasticCheck(systemSeg)){

    val mcs = minimalCutSetsBFS(model, systemSeg.observer.isOk, 4)

    /* val ur = unreliabilityMCS(model, 1E3, mcs)
    println(ur)
    mcs.foreach(println(_)) */

    interactiveSimulationB(model)
  }

}
