package com.uber.atcp.tutorialpacas.examples

import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._


object DoubleSensors extends App {
  trait DoubleSensors {
    self : System =>
    val sensors1 = Subs(nofEngines)(EngineSensor())
    val sensors2 = Subs(nofEngines)(EngineSensor())
    assertions {
      // batteries to sensors wiring
      wiring(batteries, sensors1)
      wiring(batteries, sensors2)
      // sensors sense engines
      sensors1.zip(engines).foreach { case (s,e) => s.sensed := e.state }
      sensors2.zip(engines).foreach { case (s,e) => s.sensed := e.state }
      // sensors to functions wiring
      for {
        i <- (0 until nofFunctions)
        j <- (0 until nofMonitored)
        k = i + j * nofFunctions
      } {
        functions(i).monitored(j) := sensors1(k).transmitted min sensors2(k).transmitted
      }
    }
  } 
  class Root(wiring : Wiring, nofBatteries: Int, nofEngines: Int, nofCpus: Int) 
  extends System(wiring, nofBatteries, nofEngines, nofCpus) with DoubleSensors

  val root = Root(wiring = one2all, nofBatteries = 8, nofEngines = 12, nofCpus = 4)
  for(model <- stochasticCheck(root)){
    minimalCutSequencesBFS(model, root.observer.isOk, 3)
    // interactiveSimulationB(root)
  }
}