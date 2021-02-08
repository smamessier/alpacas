package com.uber.atcp.dsl.step


import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.exprEval._
import com.uber.atcp.dsl.state._


/** @param state state variables assignement
  * @param flowDef flow assertions in topological order
  * @return completed variable assignement after flow propagation
  */
def varAssign(state : UpdateFlows, flowDef : FlowDefinition) : StableState = {
  flowDef.foldLeft(state){case (state, fdef) => state.writeFV(fdef.l, eval(fdef.r, state))}.finishUpd()
}