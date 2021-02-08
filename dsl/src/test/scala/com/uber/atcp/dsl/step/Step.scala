package com.uber.atcp.dsl.step

import com.uber.atcp.dsl._
import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.stateImpl._
import com.uber.atcp.dsl.flattening._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.exprEval._

import scala.language.implicitConversions

/** Requirements for the state data structures that will ensure the correctness of the state update 
  * when a transition is fired */
object StepTests {
  /** Writing a state value during state update should yield the corresponding value during flows update */
  def testUpdateState[T](init : StableState, svar : Expr.Svar[T], value : T) : Boolean = {
    val updState = init.startStatesUpd()
    val updatedState = updState.writeSV(svar, value)
    val state = updatedState.startFlowsUpd()

    (eval(svar, state) == value)
  }

  /** Flows should have their updated value during flows update */
  def testUpdateFlow[T](init : StableState, fvar : Expr.Fvar[T], value : T) : Boolean = {
    val updState = init.startStatesUpd().startFlowsUpd()
    val updatedState = updState.writeFV(fvar, value)
    val state = updatedState

    (eval(fvar, state) == value)
  }

  /** Reading an undefined flow should yield None */
  def testReadUndefFlow[T](init : StableState, fvar : Expr.Fvar[T]) : Boolean = {
    val updState = init.startStatesUpd().startFlowsUpd()

    (!updState.readFV(fvar).isDefined)
  }

  /** Reading state value during state update should yield its previous value */
  def testReadStateDuring[T](init : StableState, svar : Expr.Svar[T], first : T, second : T) : Boolean = {
    val updState = init.startStatesUpd().writeSV(svar, first)
    val firstState = updState.startFlowsUpd().finishUpd().startStatesUpd()
    val secondState = firstState.writeSV(svar, second)

    (secondState.readSV(svar) == first)
  }

  /** Reseting the state should yield initial values for all state variables */
  def testResetState[T](init : StableState, svar : Expr.Svar[T], value : T, m : CausalModel) : Boolean = {
    val initsv = init.readSV(svar)
    val updState = init.startStatesUpd()
    val updatedState = updState.writeSV(svar, value)
    val state = updatedState.startFlowsUpd().finishUpd().reset(m)
    initsv == state.readSV(svar)
  }

  /** Reseting the state should yield initial values for all flow variables */
  def testResetFlow[T](init : StableState, fvar : Expr.Fvar[T], value : T, m : CausalModel) : Boolean = {
    val initsv = init.readFV(fvar)
    val updFlow = init.startStatesUpd().startFlowsUpd()
    val updatedFlow = updFlow.writeFV(fvar, value)
    val state = updatedFlow.finishUpd().reset(m)
    initsv == state.readFV(fvar)
  }
}