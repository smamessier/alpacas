package com.uber.atcp.dsl.flattening

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Assertions._
import scala.language.implicitConversions

import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.assertion._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._

/** We test that the flattened model contains the same variables, assertions and transitions 
  * as the hierarchical model (checked on series parallel systems defined for MCS tests). 
  * We check that everything contained by the flattened model exists in the hierarchical model
  * and that the number of similar objects are coherent. */
class TestMCS extends AnyFlatSpec {
  def foldHierarchy[T](c : Component, convert : Component => T, aggreg : (T, T) => T) : T = {
    c.subcomps.foldLeft(convert(c)){case (r, sc) =>
      aggreg(r, foldHierarchy(sc, convert, aggreg))
    }
  }

  def containsState(c : Component, s : Expr.Svar[_]) : Boolean = {
    foldHierarchy(c, c => c.svar contains s, _||_)
  }

  def numberState(c : Component) : Int = {
    foldHierarchy(c, _.svar.size, _ + _)
  }

  def containsFlow(c : Component, f : Expr.Fvar[_]) : Boolean = {
    foldHierarchy(c, c => (c.fvar.in contains f) || (c.fvar.out contains f), _||_)
  }

  def numberFlow(c : Component) : Int = {
    foldHierarchy(c, c => c.fvar.in.size + c.fvar.out.size, _ + _)
  }

  def containsAssertion(c : Component, a : FlowAssertion[_]) : Boolean = {
    foldHierarchy(c, c => (c.assert.assertions contains a), _||_)
  }

  def numberAssertion(c : Component) : Int = {
    foldHierarchy(c, c => c.assert.assertions.size, _ + _)
  }

  val n = 4

  given Ord[Boolean] {
    def lt(x: Boolean, y: Boolean): Boolean = (!x && y)
  }

  import com.uber.atcp.dsl.mcs.SeriesParallel.{CompSpec, genAllSystems}
  val (bestMode, worstMode) = (true, false)
  val components = (1 to n).map(CompSpec(_,worstMode, InputDistribution.Exponential(1e-03))).toList
  val systems = genAllSystems(components, bestMode, worstMode)

  "State vars" should "be preserved by flattening" in {
    systems foreach { system =>
      compCheck(system).fold(
        e => assert(false, "Error during flattening"),
        model =>
          assert(model.allStateVars forall {v => containsState(system, v)}, "flattened model has unknown state var")
          assert(model.allStateVars.size == numberState(system), "state var missing from flattened model")
      )
    }
  }

  "Flow vars" should "be preserved by flattening" in {
    systems foreach { system =>
      compCheck(system).fold(
        e => assert(false, "Error during flattening"),
        model =>
          assert(model.allFlowVars forall {v => containsFlow(system, v)}, "flattened model has unknown flow var")
          assert(model.allFlowVars.size == numberFlow(system), "flow var missing from flattened model")
      )
    }
  }

  "Events" should "be preserved by flattening" in {
    systems foreach { system =>
      compCheck(system).fold(
        e => assert(false, "Error during flattening"),
        model =>
          assert(model.flowDef forall {v => containsAssertion(system, v)}, "flattened model has unknown assertion")
          assert(model.flowDef.size == numberAssertion(system), "assertion missing from flattened model")
      )
    }
  }
}
