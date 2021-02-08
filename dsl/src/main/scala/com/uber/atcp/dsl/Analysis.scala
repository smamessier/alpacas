package com.uber.atcp.dsl

import com.uber.atcp.dsl.transition.EventIds._
import com.uber.atcp.dsl.transition.Event
import com.uber.atcp.dsl.mcs._
import com.uber.atcp.dsl.stochasticSimu._
import com.uber.atcp.dsl.interactiveSimu._
import com.uber.atcp.dsl.flattening._

class MinimalSequence(val events : List[Event], private val s : String){
  override def toString = s
  override def equals(that : Any) : Boolean = {
    that match
      case m : MinimalSequence => events == m.events
      case _ => false
  }
}

object MinimalSequence {
  def apply(events : List[EventId], cm : CausalModel) : MinimalSequence =
    new MinimalSequence(
      events.map(id => cm.allTransitions(cm.idConverter.evIntIds(id)).ev), 
      events.map(e => cm.varNames.getEvent(e)).mkString(", ")
    )
}

class MinimalCutSet(val events : List[Event], private val s : String) {
  override def toString = s
  override def equals(that : Any) : Boolean = {
    that match
      case m : MinimalCutSet => events.toSet == m.events.toSet
      case _ => false
  }
}

object MinimalCutSet {
  def apply(events : List[EventId], cm : CausalModel) : MinimalCutSet =
    new MinimalCutSet(
      events.map(id => cm.allTransitions(cm.idConverter.evIntIds(id)).ev), 
      events.map(e => cm.varNames.getEvent(e)).mkString(", ")
    )
  def apply(seq : MinimalSequence) : MinimalCutSet =
    new MinimalCutSet(seq.events.toSet.toList, seq.toString)
}

object Analysis extends MCS
  with UnreliabilityMCS
  with UnbacktrackableInteractiveSimu
  with BacktrackableInteractiveSimu
  with IndicatorsSimu
  with Flattening
