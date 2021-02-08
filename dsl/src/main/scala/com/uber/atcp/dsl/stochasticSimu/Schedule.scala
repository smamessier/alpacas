package com.uber.atcp.dsl.stochasticSimu

import scala.language.implicitConversions

import com.uber.atcp.dsl.transition.EventIds._
import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.CausalModel

/** Schedule interface */
trait Schedule {
  def get(tr : EventId) : Double
  def set(tr : EventId, sc : Double) : Schedule
  def reset : Schedule
}

/** Schedule array implementation */
class MutableSchedule(private val delays : Array[Double], idConverter : IdConverter) extends Schedule {
  def get(tr : EventId) = delays(idConverter.evIntIds(tr))
  def set(tr : EventId, sc : Double) = {
    delays(idConverter.evIntIds(tr)) = sc
    this
  } 
  def reset = {
    for (i <- 0 until delays.length){
      delays(i) = 0
    }
    this
  }
}

object MutableSchedule{
  def apply(m : CausalModel) = {
    new MutableSchedule(Array.fill(m.idConverter.evNumber)(0), m.idConverter)
  }
}

/** Schedule vector implementation */
class ImmutableSchedule(private val v : Vector[Double], idConverter : IdConverter) extends Schedule{
  def get(tr : EventId) = v(idConverter.evIntIds(tr))
  def set(tr : EventId, sc : Double) = new ImmutableSchedule(v.updated(idConverter.evIntIds(tr), sc), idConverter)
  def reset = new ImmutableSchedule(Vector.fill(v.length)(0), idConverter)
}

object ImmutableSchedule{
  def apply(m : CausalModel) = {
    new ImmutableSchedule(Vector.fill(m.idConverter.evNumber)(0), m.idConverter)
  }
}