package com.uber.atcp.dsl.interactiveSimu

import org.apache.commons.math3.distribution._

import com.uber.atcp.dsl.CausalModel
import com.uber.atcp.dsl.step._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.transition.EventIds._


class TieBreakError(s : String) extends Exception(s)
class Livelock extends Exception

/** Interface for simluation step instructions */
trait LoopIteration{
  /** @param m flattened model
    * @param state current variable assignement
    * @param fireableTransitions event set for fireable transitions
    * @return Some(new state) or None if simulation ends 
    */ 
  def apply(
    m : CausalModel, 
    state : StableState, 
    fireableTransitions : EventSet, 
    fireableUrgent : EventSet
  ) : Option[StableState] 
  def printState(state : StableState, m : CausalModel) : Unit
}

/** Interactive simulation engine
  * @param li instructions to perform at each simulation step */
@scala.annotation.tailrec
def interactiveLoop(
  m : CausalModel, 
  state : StableState, 
  fireableTransitions : EventSet, 
  fireableUrgent : EventSet, 
  li : LoopIteration,
  rng : UniformRealDistribution,
  livelockCounter : Int
) : Unit = {

  updateFireable(m, state, fireableTransitions)

  li.printState(state, m)
  li(m, state, fireableTransitions, fireableUrgent) match
    case Some(st) => interactiveLoop(m, st, fireableTransitions, fireableUrgent, li, rng, 0)
    case None => 
}