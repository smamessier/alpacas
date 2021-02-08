package com.uber.atcp.dsl.frontend

import scala.collection.mutable.ArrayBuffer
import sourcecode._
import org.apache.commons.math3.distribution._

import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.transition.EventIds._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.ast.StateIds._
import com.uber.atcp.dsl.ast.FlowIds._
import com.uber.atcp.dsl.InputDistribution

trait VariablesFactory {

  /** Incoming flow variable declaration */
  object InFlow {
    def apply[T](using fvar : CompFVSet, name : sourcecode.Name, line : sourcecode.Line) : Expr.Fvar[T] = {
      val res = new Expr.Fvar[T](FlowId(), name.value, line.value)
      fvar.in += res
      res
    }
  }

  /** Incoming flow variables vector declaration */
  object InFlows {
    def apply[T](n : Int)(using fvar : CompFVSet, name : sourcecode.Name, line : sourcecode.Line) : Vector[Expr.Fvar[T]] = {
      Vector.tabulate(n){ i =>
        val res = new Expr.Fvar[T](FlowId(), name.value + s"($i)", line.value)
        fvar.in += res
        res
      }
    }
  }

  /** Outgoing flow variable declaration */
  object OutFlow {
    def apply[T](using fvar : CompFVSet, name : sourcecode.Name, line : sourcecode.Line) : Expr.Fvar[T] = {
      val res = new Expr.Fvar[T](FlowId(), name.value, line.value)
      fvar.out += res
      res
    }
  }

  /** Outgoing flow variables vector declaration */
  object OutFlows {
    def apply[T](n : Int)(using cfvs : CompFVSet, name : sourcecode.Name, line : sourcecode.Line) : Vector[Expr.Fvar[T]] = {
      Vector.tabulate(n){ i =>
        val res = new Expr.Fvar[T](FlowId(), name.value + s"($i)", line.value)
        cfvs.out += res
        res
      }
    }
  }

  /** State variable declaration */
  object State {
    def apply[T](init : T)(using svar : StateVarSet, name : sourcecode.Name, line : sourcecode.Line) : Expr.Svar[T] = {
      val res = new Expr.Svar[T](StateId(), init, name.value, line.value)
      svar += res 
      res
    }
  }

  /** State variables vector declaration */
  object States {
    def apply[T](n : Int)(f : Int => T)(using svs : StateVarSet, name : sourcecode.Name, line : sourcecode.Line) : Vector[Expr.Svar[T]] = {
      Vector.tabulate(n){i => 
        val res = new Expr.Svar[T](StateId(), f(i), name.value + s"($i)", line.value)
        svs += res
        res
      }
    }
    def apply[T](n : Int)(init : T)(using svs : StateVarSet, name : sourcecode.Name, line : sourcecode.Line) : Vector[Expr.Svar[T]] = {
      States(n)(i => init)
    }
  }

  /** Event declaration */
  object Event{
    def apply()(using name : sourcecode.Name) : Event = {
      new Event(EventId(), name.value, None, None, Policy.Restart, false)
    }
    def apply(
      distrib : InputDistribution, 
      policy : Policy = Policy.Restart
    )(using name : sourcecode.Name) : Event = {
      val internalDis = InternalDistribution(distrib)
      new Event(EventId(), name.value, Some(internalDis), None, policy, false)
    }
    def apply(
      distrib : InputDistribution, 
      weight : Double, 
    )(using name : sourcecode.Name) : Event = {
      val internalDis = InternalDistribution(distrib)
      new Event(EventId(), name.value, Some(internalDis), Some(weight), Policy.Restart, false)
    }
    def apply(
      distrib : InputDistribution, 
      weight : Double, 
      policy : Policy
    )(using name : sourcecode.Name) : Event = {
      val internalDis = InternalDistribution(distrib)
      new Event(EventId(), name.value, Some(internalDis), Some(weight), policy, false)
    }
    def urgent()(using name : sourcecode.Name) : Event = {
      new Event(EventId(), name.value, None, None, Policy.Restart, true)
    }
  }
}
