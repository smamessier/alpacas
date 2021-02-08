package com.uber.atcp.dsl.frontend

import scala.collection.mutable._
import scala.language.implicitConversions

import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.assertion._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.Component

type Subcomponents = ArrayBuffer[Component]
type StateVarSet = ArrayBuffer[Expr.Svar[_]]
type FlowVarSet = ArrayBuffer[Expr.Fvar[_]]
class CompFVSet {
  val out : FlowVarSet = new ArrayBuffer()
  val in : FlowVarSet = new ArrayBuffer()
} 



trait ComponentFactory {
  
  /** Return type for higher-order parameters producing assertions */
  type Assertions = FlowAssertions ?=> Unit

  /** Assertions declaration */
  def assertions(init : FlowAssertions ?=> Unit)(using assert : FlowAssertionBuilder) = {
    given as FlowAssertions = assert.assertions
    init
  }


  /** Subcomponents declaration */
  object Sub {
    def apply[T <: Component](s : T)(using subcomps : Subcomponents, name : sourcecode.Name) : T = {
      s.name = name.value
      subcomps += s
      s
    }

    def apply[T <: Component](s : T, i : Int)(using subcomps : Subcomponents, name : sourcecode.Name) : T = {
      val subComp = s
      subComp.name = name.value + s"($i)"
      subcomps += subComp
      subComp
    }
  }

  /** Subcomponents vectors declaration */
  object Subs {
    def apply[T <: Component](n : Int)(f : Int => T)(using subcomps : Subcomponents, name : sourcecode.Name) : Vector[T] = {
      Vector.tabulate(n){i => Sub(f(i), i)}
    }
    
    def apply[T <: Component](n : Int)(f : => T)(using subcomps : Subcomponents, name : sourcecode.Name) : Vector[T] = {
      Vector.tabulate(n){i => Sub(f, i)}
    }
  }
}
