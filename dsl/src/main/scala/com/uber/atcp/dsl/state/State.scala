package com.uber.atcp.dsl.state

import com.uber.atcp.dsl.ast.Expr
import com.uber.atcp.dsl.CausalModel

/** Variable assignements reading interface */
trait State {
  def readSV[T](s : Expr.Svar[T]): T
  def readFV[T](f : Expr.Fvar[T]): Option[T]
  def toString(m : CausalModel) : String = {
    var res = ""
    for {
      sv <- m.allStateVars
    } res += s"${m.varNames.getState(sv.uid)} : ${this.readSV(sv)}\n"
    for {
      fv <- m.allFlowVars
    } res += s"${m.varNames.getFlow(fv.uid)} : ${this.readFV(fv) match {case Some(v) => v.toString; case None => ""}}\n" 
    res
  }
}
