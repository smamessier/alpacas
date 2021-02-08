package com.uber.atcp.dsl.stochasticSimu

import scala.language.implicitConversions
import org.apache.commons.math3.distribution._
import cats.data.Validated._
import cats.implicits._

import com.uber.atcp.dsl.frontend._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl._
import com.uber.atcp.dsl.step._
import com.uber.atcp.dsl.state._
import com.uber.atcp.dsl.flattening._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.transition.EventIds._
import com.uber.atcp.dsl.exprEval._
import com.uber.atcp.dsl.errors._
import com.uber.atcp.dsl.stateImpl._

/** Recursive function firing one transition per iteration
  * observer has acces to current aggregate, current state, whether it is a deadlock, time in state, total time
  * @tparam T aggregate result type
  * @param m flattened model
  * @param state current state 
  * @param schedule previous schedule
  * @param step duration spent in previous step
  * @param rng Random Number Generator for conflict resolution
  * @param toFire set of events to fire (with conflicts)
  * @param lastFireable set of events corresponding to fireable transitions
  * @param observer function for result aggregation, stop condition, restart condition
  * @param accu aggregate result
  * @param livelockCounter number of consecutive steps of duration 0
  * @return new aggregate result or error if impossible tie break, total time 
  */
@scala.annotation.tailrec
def simulationLoop[T](
  m : StochasticModel, 
  state : StableState, 
  schedule : Schedule, 
  step : Double,
  rng : UniformRealDistribution, 
  toFire : EventSet,
  lastFireable : EventSet,
  fireableUrg : EventSet,
  observer : (T, StableState, Boolean, Double, Double) => (T, Boolean),
  totalTime : Double,
  accu : T,
  livelockCounter : Int,
) : EitherResult[(T, Double)] = {
  /* Compute new clock values and minimal delay events */
  val (newSchedule, newStep) = updateSchedule(m, state, toFire, lastFireable, schedule, step)
  val newLivelockCounter = if newStep == 0 then livelockCounter + 1 else 0
  val nbMinEv = toFire.size
  val newTime = totalTime + newStep
  if (nbMinEv == 0) // deadlock state
    val (newAccu, stopCond) = observer(accu, state, true, newStep, newTime) 
    Right((newAccu, newTime))
  else // fire next transition
    val ev : EitherResult[EventId] = nbMinEv match
      case 1 => // no race condition -> nominal case
        Right(toFire.head)
      case _ => // race condition -> tie break using weights
        val rd = rng.sample()
        val fireVect = toFire.toVector
        tieBreak(m, fireVect, rd)

    /* call to observer for stats update and stop condition */
    val (newAccu, stopCond) = observer(accu, state, false, newStep, newTime) // time spent in state is newStep 
    if (stopCond)
      Right((newAccu, newTime))
    else
      /* livelock detection and terminate */
      if newLivelockCounter > 1000 then // threshold at 1000 instant transitions 
        LiveLock().invalidNec.toEither
      else
        ev match
          case Right(e) =>
            fire(m, state, e, fireableUrg) match
              case Right(newState) =>
                simulationLoop(
                  m, 
                  newState, 
                  newSchedule, 
                  newStep, 
                  rng, 
                  toFire, 
                  lastFireable, 
                  fireableUrg, 
                  observer, 
                  newTime, 
                  newAccu, 
                  newLivelockCounter
                )
              case Left(e) => Left(e)
          case Left(e) => Left(e)
}

/** Schedule update for fireable transition */
def handleFireable(
  newClock : Double,
  t : Transition,
  toFire : EventSet,
  minDelay : Double
) : (Double, Double) = { 
  if (newClock < minDelay)
    toFire.clear
    toFire.add(t.ev.id)
    (newClock, newClock)
  else
    if (newClock == minDelay) then toFire.add(t.ev.id)
    (newClock, minDelay)
}

/** Schedule update for transition becoming fireable */
def handleBecomesFireable(
  newDelay : Double, 
  t : Transition, 
  toFire : EventSet, 
  minDelay : Double
) : (Double, Double) = { 
  val newClock = if (newDelay == 0 || t.ev.policy == Policy.Restart) then t.ev.sample() else newDelay
  handleFireable(newClock, t, toFire, minDelay)
}

/** Schedule update and guard evaluation for transition
  * @param t Transition to update
  * @param state Current state
  * @param schedule Clock values for all transitions (partially updated)
  * @param step Time progression for this update
  * @param toFire Set of transitions to fire (partially updated)
  * @param lastFireable Set of transitions fireable in the last state (partially updated)
  * @param minDelay Minimal delay for already updated transitions
  * @return New clock value for transition, New minDelay
  */
def handleTransition(
  t : Transition, 
  state : State, 
  schedule : Schedule, 
  step : Double, 
  toFire : EventSet, 
  lastFireable : EventSet,
  minDelay : Double,
) : (Double, Double) = {
  val d = schedule.get(t.ev.id)
  if eval(t.guard, state)
    val res = 
      if lastFireable.contains(t.ev.id)
        handleFireable(d - step, t, toFire, minDelay)
      else
        handleBecomesFireable(d, t, toFire, minDelay)
    lastFireable.add(t.ev.id)
    res
  else
    val res = 
      if lastFireable.contains(t.ev.id)
        (d - step, minDelay)
      else
        (d, minDelay)
    lastFireable.remove(t.ev.id)
    res
}

/** Schedule update and determination of minimal delay (with corresponding transitions)
  * @param m Flattened system model
  * @param state Current state
  * @param toFire EventSet to store fireable transitions with minimal delay
  * @param lastFireable Fireable transitions in last state
  * @param step Clock step
  * @return updated schedule, minimal delay
  */
def updateSchedule(
  m : CausalModel, 
  state : State, 
  toFire : EventSet,  
  lastFireable : EventSet,
  schedule : Schedule, 
  step : Double,
) : (Schedule, Double) = {
  toFire.clear
  m.allTransitions.toList.foldLeft(schedule, Double.PositiveInfinity){
    case ((schedule, minDelay), t) => 
      val (trSchedule, newMinDelay) = handleTransition(t, state, schedule, step, toFire, lastFireable, minDelay)
      (schedule.set(t.ev.id, trSchedule), newMinDelay)
  }
}

/** Batch of stochastic simulation evaluation and result aggregation
  * Potential errors are printed and stop simulation 
  * @tparam T simulation aggregate result type
  * @tparam U batch aggregate result type
  * @param m top-level component
  * @param observer Result aggregating and stop condition in function of previous result, state, deadlock, state time, total time
  * @param simulationAggregator result aggregating function for simulations batch
  * @param batchSize number of simulations in the batch
  * @param first simulation result initial value
  * @param firstAg batch result initial value
  * @return aggregate result at the end of simulation, total time
  */
def stochasticSimulationBatch[T, U](
  model : StochasticModel, 
  observer : (T, StableState, Boolean, Double, Double) => (T, Boolean),
  simulationAggregator : (U, T, Double) => U,
  batchSize : Int,
  first : T,
  firstAg : U,
) : EitherResult[U] = {
  val state = mutableInitialState(model)
  val toFire = EventSet(model)
  val lastFireable = EventSet(model)
  val fireableUrg = EventSet(model)
  val schedule = MutableSchedule(model)
  (0 until batchSize).foldLeft(Right(firstAg) : EitherResult[U]){ case (resAgEither, _) =>
    resAgEither flatMap { resAg =>
      toFire.clear
      lastFireable.clear
      val resSimu = 
        simulationLoop(
          model, 
          state.reset(model), 
          schedule.reset, 
          0, 
          UniformRealDistribution(), 
          toFire, 
          lastFireable, 
          fireableUrg,
          observer, 
          0.0, 
          first, 
          0
        ) 
      resSimu.flatMap{case (v, t) => 
        Right(simulationAggregator(resAg, v, t))
      }
    }
  }
}

/** Batch of stochastic simulation evaluation and result aggregation
  * Potential errors are printed and stop simulation 
  * @tparam T simulation aggregate result type
  * @tparam U batch aggregate result type
  * @param m top-level component
  * @param observer Result aggregating and stop condition in function of previous result, state, deadlock, state time, total time
  * @param simulationAggregator result aggregating function for simulations batch
  * @param batchSize number of simulations in the batch
  * @param first simulation result initial value
  * @param firstAg batch result initial value
  * @return aggregate result at the end of simulation, total time
  */
def stochasticSimulation[T, U](
  model : StochasticModel, 
  observer : (T, StableState, Boolean, Double, Double) => (T, Boolean),
  first : T,
) : EitherResult[(T, Double)] = {
  val state = mutableInitialState(model)
  val toFire = EventSet(model)
  val lastFireable = EventSet(model)
  val fireableUrg = EventSet(model)
  val schedule = MutableSchedule(model)
  val resSimu = 
    simulationLoop(
      model, 
      state, 
      schedule, 
      0, 
      UniformRealDistribution(), 
      toFire, 
      lastFireable, 
      fireableUrg,
      observer, 
      0.0, 
      first, 
      0
    ) 
  resSimu
}