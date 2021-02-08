package com.uber.atcp.dsl.step

import com.uber.atcp.dsl.state._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.CausalModel

/** Read-only assignement */
trait StableState extends State {
  def startStatesUpd(): UpdateStates
  def reset(model : CausalModel) : StableState = 
    val updSt = this.startStatesUpd()
    val updatedState = model.allStateVars.foldLeft(updSt){case (res, sv@Expr.Svar(uid, init, name, line)) => 
      res.writeSV(sv, init)
    }
    varAssign(updatedState.startFlowsUpd(), model.flowDef)
}

/** Interface for state vars update */
trait UpdateStates extends State {
  def writeSV[T](s: Expr.Svar[T], v: T): UpdateStates
  def startFlowsUpd(): UpdateFlows
}

/** Interface for the flow vars update */
trait UpdateFlows extends State {
  def writeFV[T](s: Expr.Fvar[T], v: T): UpdateFlows
  def finishUpd(): StableState
}
