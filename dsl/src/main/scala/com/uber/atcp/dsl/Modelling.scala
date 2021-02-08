package com.uber.atcp.dsl

import scala.collection.mutable.ArrayBuffer

import com.uber.atcp.dsl.frontend._
import com.uber.atcp.dsl.assertion._


object Modelling extends ComponentFactory 
  with VariablesFactory 
  with ExpressionFactory 
  with TransitionFactory

/** Extended by all component definitions */
class Component {
  given assert as FlowAssertionBuilder(FlowAssertions())
  given trans as TransitionBuilder(Transitions())
  given fvar as CompFVSet = new CompFVSet()
  given svar as StateVarSet = new ArrayBuffer()
  given subcomps as Subcomponents = new ArrayBuffer()

  var name = ""
  var fullName = ""

  override def toString = {
    assert.assertions.mkString("AssertionBuilder :\n", ",\n", "\n") + 
    trans.transitions.mkString("Transitions :\n",",\n","\n") +
    fvar.in.mkString("In Flow variables : ", ", ", "\n") +
    fvar.out.mkString("Out Flow variables : ", ", ", "\n") +
    svar.mkString("State variables : ", ", ", "\n") +
    s"Number of Subcomponents : ${subcomps.size}"
  }
}

/** Input format for event probability distribution */
enum InputDistribution {
  case Exponential(lambda : Double)
  case Dirac(delta : Double)
}

import Modelling.{_, given _}

/** Urgent delay (used to build causal feedback loops). */
class Delay[A: Lifted](init: A) extends Component {
  val in = InFlow[A]
  val out = OutFlow[A]
  val state = State[A](init = init)
  val propagate = Event.urgent() 
  transitions { When(propagate) If (!(in === state)) Then { state := in } }
  assertions { out := state }
}
