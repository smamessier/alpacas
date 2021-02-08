package com.uber.atcp.dsl.flattening

import com.uber.atcp.dsl.Component
import com.uber.atcp.dsl.Modelling.{given _}
import com.uber.atcp.dsl.frontend._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.ast.Expr
import com.uber.atcp.dsl.causalModel._


def apply(v : Vector[InputTransition]) : Vector[Transition] = {
  v.map{
    case InputTransition(e, Synchronizable.Explicit(g, a)::Nil) => simplify(Transition(e, g, a))
  }
}

/** Synchro flattening function
  * Recursive flattening of optional and mandatory events
  * Application of synchro flattening rules
  */
def flattenSynchro(v : Vector[InputTransition], id : Int, idConv : IdConverter) : Vector[InputTransition] = {
  val InputTransition(e, trs) = v(id) 
  trs match // Maybe we could get rid of this, subcase of general case, test...
    case Synchronizable.Explicit(_, _)::Nil => v
    case _ => 
      val newVect = trs.foldLeft(v){
        case (vres, Synchronizable.Mandatory(e)) => flattenSynchro(vres, idConv.evIntIds(e.id), idConv)
        case (vres, Synchronizable.Optional(e)) => flattenSynchro(vres, idConv.evIntIds(e.id), idConv)
        case (vres, _) => vres
      }
      val mandatories = trs collect {case Synchronizable.Mandatory(e) => 
        val InputTransition(_, Synchronizable.Explicit(g, a)::Nil) = newVect(idConv.evIntIds(e.id))
        Transition(e, g, a)
      }
      val optionals = trs collect {case Synchronizable.Optional(e) => 
        val InputTransition(_, Synchronizable.Explicit(g, a)::Nil) = newVect(idConv.evIntIds(e.id))
        Transition(e, g, a)
      } 
      val anons = trs collect {case t @ Synchronizable.Explicit(_, _) => t} 
      val a = StateAssertionBuilder()
      if (mandatories.isEmpty && anons.isEmpty)
        val g = optionals.map(_.guard).reduce(_||_)
        optionals.foreach{t => 
          t.a foreach {case StateAssertion(l, r, line) => 
            a += StateAssertion(l, Expr.Ite(t.guard, r, l), line)
          }
        }
        val expl = 
          if (optionals.size > 1) 
            Synchronizable.Explicit(g, a) 
          else 
            Synchronizable.Explicit(Expr.Const(true), a)
        val tr = InputTransition(e, expl::Nil)
        v.updated(idConv.evIntIds(e.id), tr)
      else
        val g = (mandatories.map(_.guard) ++ anons.map(_.guard)).reduce(_&&_)
        anons.foreach{t => t.assertions.foreach{a += _}}
        optionals.foreach{t => 
          t.a foreach {case StateAssertion(l, r, line) => 
            a += StateAssertion(l, Expr.Ite(t.guard, r, l), line)
          }
        }
        mandatories.foreach{t => t.a foreach {a += _}}
        val tr = InputTransition(e, Synchronizable.Explicit(g, a)::Nil)
        v.updated(idConv.evIntIds(e.id), tr)
}