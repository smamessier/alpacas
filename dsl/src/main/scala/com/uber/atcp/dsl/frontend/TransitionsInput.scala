package com.uber.atcp.dsl.frontend

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.ast._

/** Internal transition representation before flattening */
case class InputTransition (event : Event, trs : List[Synchronizable]) {
  override def toString = s"Tr on ${event.id} : ${trs.mkString(", ")}"
}

type Transitions = ArrayBuffer[InputTransition]
case class TransitionBuilder(transitions : Transitions)

case class Whent(ev : Event)

case class AnonymousSync(ev : Event, guard : Expr[Boolean], assertions : StateAssertionBuilder){
  def With(s : Synchro)(using trans : Transitions) = {
    trans += InputTransition(this.ev, Synchronizable.Explicit(this.guard, this.assertions)::s.syncs)
  }
}

/** Internal representation of transitions / synchro before flattening */
enum Synchronizable {
  case Mandatory(ev : Event) 
  case Optional(ev : Event)
  case Explicit(guard : Expr[Boolean], assertions : StateAssertionBuilder)
}

case class Synchro(syncs : List[Synchronizable])

case class GuardedEvent(ev : Event, guard : Expr[Boolean])

case class Synct(ev : Event)

case class GuardedSync(ev : Event, guard : Expr[Boolean])


trait TransitionFactory {
  def When(ev : Event) : Whent = {
    Whent(ev)
  }

  /** Wrapper for events after "When" in transition declaration */
  extension (w : Whent){
    def If(g : Expr[Boolean]) : GuardedEvent = {
      GuardedEvent(w.ev, g)
    }
    def Then(a : StateAssertionBuilder ?=> Unit)(using trans : Transitions) = {
      given assert as StateAssertionBuilder
      a
      trans += InputTransition(w.ev, List(Synchronizable.Explicit(Expr.Const(true), assert)))
    }
  }

  def Sync(ev : Event) : Synct = {
    Synct(ev)
  }

  extension (ge : GuardedEvent)
    def Then(a : StateAssertionBuilder ?=> Unit)(using trans : Transitions) = {
      given assert as StateAssertionBuilder
      a
      trans += InputTransition(ge.ev, List(Synchronizable.Explicit(ge.guard, assert)))
    }

  /** Wrapper for event after "Sync" in synchro declaration */
  extension (st : Synct){
    def If(g : Expr[Boolean]) : GuardedSync = GuardedSync(st.ev, g)
    def With(s : Synchro)(using trans : Transitions) = {
      trans += InputTransition(st.ev, s.syncs)
    }
  }

  extension (gs : GuardedSync){
    def Then(a : StateAssertionBuilder ?=> Unit) : AnonymousSync = {
      given assert as StateAssertionBuilder
      a
      AnonymousSync(gs.ev, gs.guard, assert)
    }
    def With(s : Synchro)(using trans : Transitions) = {
      trans += InputTransition(gs.ev, Synchronizable.Explicit(gs.guard, StateAssertionBuilder())::s.syncs)
    }
  }

  extension (l : Synchro)
    def & (r : Synchro) : Synchro = Synchro(l.syncs ++ r.syncs)

  extension (e : Event) {
    def hard : Synchro = Synchro(Synchronizable.Mandatory(e)::Nil)
    def soft : Synchro = Synchro(Synchronizable.Optional(e)::Nil)
  }

  /** Transitions declaration */
  def transitions(init : Transitions ?=> Unit)(using trans : TransitionBuilder) = {
    given as Transitions = trans.transitions
    init
  }
}