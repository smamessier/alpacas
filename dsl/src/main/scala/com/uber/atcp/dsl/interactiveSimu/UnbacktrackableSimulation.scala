package com.uber.atcp.dsl.interactiveSimu

import org.apache.commons.math3.distribution._

import com.uber.atcp.dsl._
import scala.language.implicitConversions
import scala.collection.mutable.BitSet
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.step._
import com.uber.atcp.dsl.stateImpl._


/** Unbacktrackable simulation step instructions */
object UnbacktrackableLI extends LoopIteration {
  def apply(
    m : CausalModel, 
    state : StableState, 
    fireableTransitions : EventSet, 
    fireableUrgent : EventSet
  ) : Option[StableState] = {
    println("Enter the id of the transition to fire among the following list or exit to end simulation")
    val ftVector = fireableTransitions.toVector
    for (i <- 0 until ftVector.size){
      println(s"$i : ${m.varNames.getEvent(ftVector(i))}")
    }
    val input = scala.io.StdIn.readLine()
    if (input == "exit")
      println("End of simulation")
      None
    else if (0 <= input.toInt && input.toInt < ftVector.size)
      fire(m, state, ftVector(input.toInt), fireableUrgent)
      Some(state)
    else
      println("Invalid input")
      Some(state)
  }
  def printState(state : StableState, m : CausalModel) : Unit = {
    println(state.toString(m))
  }
}


trait UnbacktrackableInteractiveSimu {

  /** Unbacktrackable interactive simulation user trigger function
    * Performs checks and starts simulation */
  def interactiveSimulationUB(model : CausalModel) = {
    val state = mutableInitialState(model)
    interactiveLoop(model, state, EventSet(model), EventSet(model), UnbacktrackableLI, UniformRealDistribution(), 0)
  }

}
