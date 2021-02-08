package com.uber.atcp.dsl.step

import scala.collection.mutable.BitSet

import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.transition.EventIds._
import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.CausalModel


/** Set of events interface 
  * Bitsets implementation */
class EventSet(private val bitSet : BitSet, private val idConv : IdConverter, private val events : Vector[EventId]) {
  def add(ev : EventId) : Unit = bitSet.add(idConv.evIntIds(ev))
  def remove(ev : EventId) : Unit = bitSet.remove(idConv.evIntIds(ev))
  def clear : Unit = bitSet.clear
  def toVector : Vector[EventId] = bitSet.toVector.map(i => events(i))
  def size : Int = bitSet.size
  def head : EventId = events(bitSet.head)
  def contains(ev : EventId) : Boolean = bitSet(idConv.evIntIds(ev))
}

object EventSet {
  def apply(m : CausalModel) : EventSet = 
    new EventSet(new BitSet(m.idConverter.evNumber), m.idConverter, m.allTransitions.map(_.ev.id))
}
