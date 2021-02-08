package com.uber.atcp.dsl

import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.ast._

/** Full flattened model 
  * @param c top level component
  * @param flowDef assertion definition for every flow variable
  * @param allNonUrgentTransitions all non urgent transitions 
  * @param allUrgentTransitions all urgent transitions 
  * @param allStateVars all state vars (by int state ids)
  * @param allFlowVars all flow vars (by int flow ids)
  * @param allTransitions all transitions (by event int ids)
  * @param idConverter mapings from uuids to int ids
  * @param varNames full names of states flows and events (by uuids)
*/
class CausalModel(
  val c : Component, 
  val flowDef : FlowDefinition, 
  val allNonUrgentTransitions : List[Transition], 
  val allUrgentTransitions : List[Transition], 
  val allStateVars : Vector[Expr.Svar[_]], 
  val allFlowVars : Vector[Expr.Fvar[_]],
  val allTransitions : Vector[Transition],
  val idConverter : IdConverter,
  val varNames : VarNames,
)

class StochasticModel(
  c : Component, 
  flowDef : FlowDefinition, 
  allNonUrgentTransitions : List[Transition], 
  allUrgentTransitions : List[Transition], 
  allStateVars : Vector[Expr.Svar[_]], 
  allFlowVars : Vector[Expr.Fvar[_]],
  allTransitions : Vector[Transition],
  idConverter : IdConverter,
  varNames : VarNames,
) extends CausalModel(
  c,
  flowDef,
  allNonUrgentTransitions,
  allUrgentTransitions,
  allStateVars,
  allFlowVars,
  allTransitions,
  idConverter,
  varNames
)
