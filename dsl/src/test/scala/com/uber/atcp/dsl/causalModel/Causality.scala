package com.uber.atcp.dsl.causalModel

import cats.data.Validated._
import cats.data._
import scala.language.implicitConversions

import com.uber.atcp.dsl.flattening._
import com.uber.atcp.dsl.errors._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.CausalModel

/** We implement functions to check if a given order is a topological order for the dependence relation. 
  * And to check if the definitions of a list of variables are cyclic. */
object CausalityTests {

  /** Checks whether the topological order in a causal model respects a given dependency relation
    * @param cm causal model from flattening
    * @param dependencies map from variables to their dependencies (represented as strings)
    * @return true if the topological order is correct
    */
  def checkTopo(cm : CausalModel, dependencies : Map[String, Set[String]]) : Boolean = {
    val topoOrder = cm.flowDef.map(_.l.name)
    topoOrder.foldLeft((true, Set[String]())){case ((res, forbidden), current) =>
      val newForbidden = forbidden.union(dependencies(current))
      (res && !newForbidden.contains(current), newForbidden)
    }._1
  }

  /** Checks that the errors from flattening correspond to the dependencies */
  def checkErrors(err : NonEmptyChain[AlpacasError], dependencies : Map[String, Set[String]]) : Boolean = {
    err.map{e => e match
      case SelfDependent(name) =>
        dependencies(name).contains(name) 
      case CyclicDefinition(names) =>
        val lastNames = names.tail
        val firstNames = names.dropRight(1)
        (lastNames zip firstNames).map{case (name, prev) => 
          dependencies(prev).contains(name)
        }.reduce(_ && _)
      case _ => false
    }.reduce(_ && _)
  }

  /** Set of variables contained in an expression (as strings) */
  def variables[T](e : Expr[T]) : Set[String] = {
    e match
      case Expr.Const(_) | Expr.Svar(_, _, _, _)=> Set()
      case Expr.Fvar(_, name, _) => Set(name)
      case Expr.Eq(l, r) => variables(l).union(variables(r))
      case Expr.Ite(c, l, r) => variables(l).union(variables(r)).union(variables(c))
      case Expr.Lt(l, r) => variables(l).union(variables(r))
      case Expr.NumBin(_, l, r) => variables(l).union(variables(r))
      case Expr.LogBin(_, l, r) => variables(l).union(variables(r))
      case Expr.Un(_, e) => variables(e)
  }

}