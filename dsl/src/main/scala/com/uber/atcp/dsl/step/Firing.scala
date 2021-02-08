package com.uber.atcp.dsl.step

import scala.collection.mutable.Queue
import cats.data.Validated._
import cats.implicits._
import scala.language.implicitConversions

import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.state._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.transition.EventIds._
import com.uber.atcp.dsl.CausalModel
import com.uber.atcp.dsl.exprEval._
import com.uber.atcp.dsl.errors._


/** @param m flattened model
  * @param state current variable assignement
  * @return list of fireable transitions
  */
def fireable(m: CausalModel, state: State): List[Event] = {
  val res = for {
    t <- m.allTransitions.toList
    if eval(t.guard, state) && !t.ev.hidden
  } yield t.ev
  res
} 

/** Fireable transitions set updated to current state */
def updateFireable(m : CausalModel, state : State, fireableTr : EventSet) : Unit = {
  for (t <- m.allTransitions.toList) {
    if eval(t.guard, state) && !t.ev.hidden
      fireableTr.add(t.ev.id) 
    else 
      fireableTr.remove(t.ev.id)
  }
}

/** @param m flattened model 
  * @param state current state
  * @param ev event to fire
  * @return new state after transition firing
  */
def fireOne(m: CausalModel, state: StableState, ev: EventId): StableState = {
  def updateStateVars(state : UpdateStates, t : Transition) : UpdateStates = {
    t.a.foldLeft(state){case (state, StateAssertion(l, r, line)) => 
      state.writeSV(l, eval(r, state))
    }
  }
  val tr = m.allTransitions(m.idConverter.evIntIds(ev))

  if eval(tr.guard, state)
    val updatedState = updateStateVars(state.startStatesUpd(), tr).startFlowsUpd()
    varAssign(updatedState, m.flowDef)
  else
    state // if the transition is not fireable, we return the unmodified state
}

def propagateUrgent(m : CausalModel, state : StableState, fireableUrg : EventSet): EitherResult[StableState] = {

  def successorsBFS(state : StableState) : EitherResult[Set[StableState]] = {
    val queue = Queue(state)
    var res : Set[StableState] = Set()
    var explored = 0
    while (!queue.isEmpty && explored < 1000) {
      explored += 1
      val state = queue.dequeue()
      var stable = true
      m.allUrgentTransitions.foreach{t =>
        if eval(t.guard, state)
          stable = false
          queue.enqueue(fireOne(m, state, t.ev.id))
      }
      if stable
        res = res + state
    }
    if (explored < 1000)
      Right(res)
    else 
      DivergingUrgentUnfolding(state.toString(m)).invalidNec.toEither
  }

  successorsBFS(state).flatMap{nextStates =>
    if nextStates.size == 1
      Right(nextStates.head)
    else 
      DivergingUrgentUnfolding(state.toString(m)).invalidNec.toEither
  }
}

def fire(m: CausalModel, state: StableState, ev: EventId, fireableUrg : EventSet): EitherResult[StableState] = {
  val newState = fireOne(m, state, ev)
  propagateUrgent(m, newState, fireableUrg)
}
