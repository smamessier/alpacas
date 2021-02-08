package com.uber.atcp.tutorialpacas.examples


import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._

import scala.language.implicitConversions  
import scala.language.postfixOps

object SingleSensors extends App {

  trait SingleSensors {
    self : System =>
    val sensors = Subs(nofEngines)(EngineSensor())
    assertions {
      // batteries to sensors wiring
      wiring(batteries, sensors)
      // sensors sense engines
      sensors.zip(engines).foreach { case (s,e) => s.sensed := e.state }
      // sensors to functions wiring
      for {
        i <- (0 until nofFunctions)
        j <- (0 until nofMonitored)
        k = i + j * nofFunctions
      } {
        functions(i).monitored(j) := sensors(k).transmitted 
      }
    }
  } 

  class Root(wiring : Wiring, nofBatteries: Int, nofEngines: Int, nofCpus: Int) 
  extends System(wiring, nofBatteries, nofEngines, nofCpus) with SingleSensors
  
  val root = Root(wiring = one2all, nofBatteries = 8, nofEngines = 12, nofCpus = 4)
  for(model <- stochasticCheck(root)){
   /*  val cutSets = minimalCutSetsBFS(model, root.observer.isOk, 3)
    cutSets.foreach(println(_))
    println(cutSets.size)

    val seqs = minimalCutSequencesBFS(model, root.observer.isOk, 3)
    seqs.foreach(println(_))
    println(seqs.size) */
    // interactiveSimulationB(root)
  }
}