package com.uber.atcp.dsl.interactiveSimu

import org.apache.commons.math3.distribution._

import scala.language.implicitConversions
import scala.collection.mutable.Stack

import com.uber.atcp.dsl._
import com.uber.atcp.dsl.frontend._
import com.uber.atcp.dsl.ast.Expr 
import com.uber.atcp.dsl.assertion._
import com.uber.atcp.dsl.state._
import com.uber.atcp.dsl.step._
import com.uber.atcp.dsl.stateImpl._


/** Backtrackable simulation step instructions */
private class BacktrackableLI(private val stack : Stack[StableState]) extends LoopIteration {
  def apply(
    m : CausalModel, 
    state : StableState, 
    fireableTransitions : EventSet,
    fireableUrgent : EventSet
  ) : Option[StableState] = {
    println("Enter the id of the transition to fire among the following list")
    println("or -1 to backtrack or exit to end simulation")
    val ftVector = fireableTransitions.toVector
    for (i <- 0 until ftVector.size){
      println(s"$i : ${m.varNames.getEvent(ftVector(i))}")
    }
    val input = scala.io.StdIn.readLine()
    if (input == "exit")
      println("End of simulation")
      None
    else if (0 <= input.toInt && input.toInt < ftVector.size)
      fire(m, state, ftVector(input.toInt), fireableUrgent) match
        case Right(newState) =>
          stack.push(state)
          Some(newState)
        case Left(e) =>
          throw e.head
    else if (input.toInt == -1)
      if (stack.isEmpty)
        println("You're already back to the initial state")
        Some(state)
      else Some(stack.pop())
    else
      println("Invalid input")
      Some(state)
  }
  def printState(state : StableState, m : CausalModel) : Unit = {
    if stack.isEmpty
      println(state.toString(m))
    else
      val previousState = stack.head
      println("\nVariables modified since previous state :")
      for {
        sv <- m.allStateVars
        if state.readSV(sv) != previousState.readSV(sv)
      } println(s"${m.varNames.getState(sv.uid)} : ${state.readSV(sv)}")
      for {
        fv <- m.allFlowVars
        if state.readFV(fv) != previousState.readFV(fv)
      } println(s"${m.varNames.getFlow(fv.uid)} : ${state.readFV(fv) match {case Some(v) => v.toString; case None => ""}}")
  }
}


trait BacktrackableInteractiveSimu {
    
  /** Backtrackable interactive simulation user trigger function
    * Performs checks and starts simulation */
  def interactiveSimulationB(model : CausalModel) = {
    val state = immutableInitialState(model)
    var s = Stack[StableState]()
    interactiveLoop(model, state, EventSet(model), EventSet(model), BacktrackableLI(s), UniformRealDistribution(), 0)
  }
}
