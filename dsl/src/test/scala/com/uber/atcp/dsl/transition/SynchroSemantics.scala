package com.uber.atcp.dsl.transition

import com.uber.atcp.dsl.state._
import com.uber.atcp.dsl.step._
import com.uber.atcp.dsl.transition.EventIds._
import com.uber.atcp.dsl.frontend._
import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.exprEval._
import com.uber.atcp.dsl._

class SynchronizedNotFound(eventId : EventId) extends Exception(eventId.toString)

/** We define an alternate semantics of synchronizations in terms of 
  * whether it should be fireable in a given state, 
  * and what the resulting state should be after it is fired.
  */
object SynchroTests {

  def findTransition(m : Component, ev : EventId) : Option[InputTransition] = {
    m.trans.transitions.find(t => t.event.id == ev) match
      case Some(t) => Some(t)
      case None => 
        val res = for {
          sub <- m.subcomps
          t = findTransition(sub, ev)
          if t.isDefined
        } yield t
        if res.isEmpty
          None
        else 
          res.head
  }

  def findTransitionUnsafe(m : Component, ev : EventId) : InputTransition = {
    findTransition(m, ev) match 
      case Some(t) =>  t
      case None => throw SynchronizedNotFound(ev)
  }

  /** Checks whether a given synchro of unflattened model is fireable in a given state */
  def fireableSynchro(state : StableState, ev : EventId, m : Component) : Boolean = {
    val t = findTransitionUnsafe(m, ev)
    val hards = t.trs.collect{
      case Synchronizable.Mandatory(ev) => 
        fireableSynchro(state, ev.id, m)
      case Synchronizable.Explicit(g, _) => eval(g, state)
    }
    val softs = t.trs.collect{
      case Synchronizable.Optional(ev) => 
        fireableSynchro(state, ev.id, m)
    }
    if !hards.isEmpty
      hards.reduce(_&&_)
    else if softs.length > 1
      softs.reduce(_||_)
    else
      true
  }

  /** Computes the resulting state after firing a synchro on unflattened model */
  def synchroStep(
    state : StableState, 
    ev : EventId, 
    m : Component, 
    flowDef : FlowDefinition
  ) : StableState = {

    def updateStateVars(state : UpdateStates, a : StateAssertionBuilder) : UpdateStates = {
      a.foldLeft(state){case (state, StateAssertion(l, r, line)) => 
        state.writeSV(l, eval(r, state))
      }
    }

    val t = findTransitionUnsafe(m, ev)

    t.trs.foldLeft(state){case (newState, t) => t match
      case Synchronizable.Mandatory(ev) => 
        synchroStep(newState, ev.id, m, flowDef)
      case Synchronizable.Optional(ev) => 
        if fireableSynchro(state, ev.id, m)
          synchroStep(newState, ev.id, m, flowDef)
        else
          newState
      case Synchronizable.Explicit(_, a) =>
        val updatedState = updateStateVars(newState.startStatesUpd(), a).startFlowsUpd()
        varAssign(updatedState, flowDef)
    }
  }

}