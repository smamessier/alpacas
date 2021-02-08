package com.uber.atcp.dsl.transition

import scala.collection.mutable._
import org.apache.commons.math3.distribution._

import java.util.UUID

import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.InputDistribution


/** State assertion representation
  */
case class StateAssertion[T](l : Expr.Svar[T], r : Expr[T], line : Int) {
  override def toString = s"Assert(\n${l.toString(1)},\n${r.toString(1)})"
}

type StateAssertionBuilder = ArrayBuffer[StateAssertion[_]]


case class Transition(ev : Event, guard : Expr[Boolean], a : StateAssertionBuilder) 

/** Internal representation of event distribution */
enum InternalDistribution {
  case Exponential(sampler : ExponentialDistribution)
  case Dirac(delta : Double)

  /** @return sampled value for distribution */
  def sample() : Double = this match
    case Exponential(sampler) => sampler.sample()
    case Dirac(d) => d
}

object InternalDistribution {
  def apply(input : InputDistribution) : InternalDistribution = {
    input match
      case InputDistribution.Exponential(lambda) => 
        InternalDistribution.Exponential(ExponentialDistribution(1 / lambda)) // math3 works with mean rather than lambda
      case InputDistribution.Dirac(d) => 
        InternalDistribution.Dirac(d)
  }
}

enum Policy {
  case Memory
  case Restart
}

object EventIds {
  opaque type EventId = UUID

  object EventId{
    def apply() : EventId = UUID.randomUUID()
  }
}

import EventIds._

class Event(
  val id : EventId,
  val name : String,
  val distrib : Option[InternalDistribution],
  val weight : Option[Double],
  val policy : Policy,
  val urgent : Boolean,
){
  var hidden : Boolean = false
  def hide : Unit = hidden = true
  def sample() : Double = distrib match
    case Some(dis) => dis.sample()
    case None => throw UndefinedDistribution()
  override def toString = s"Event(${id}, $name)"
  override def equals(that : Any) = 
    that match
      case t : Event => t.id == this.id
      case _ => false
}

case class UndefinedDistribution() extends Error