package com.uber.atcp.dsl.stateImpl

import com.uber.atcp.dsl.ast.Expr
import com.uber.atcp.dsl.ast.FlowIds._
import com.uber.atcp.dsl.ast.StateIds._
import com.uber.atcp.dsl.state._
import com.uber.atcp.dsl.step._
import com.uber.atcp.dsl.CausalModel
import com.uber.atcp.dsl.causalModel._

/** Variable assignement vector implementation */
class ImmutableState(
  private val st : Vector[Any], 
  private val fl : Vector[Option[Any]],
  private val idCv : IdConverter
) extends State {
  def readSV[T](s : Expr.Svar[T]): T = st(idCv.stateIntIds(s.uid)).asInstanceOf[T]
  def readFV[T](f : Expr.Fvar[T]): Option[T] = fl(idCv.flowIntIds(f.uid)).flatMap{t => Some(t.asInstanceOf[T])}
}

/** Read-only assignement vector implementation */
class ImmutableStableState(
  private val st : Vector[Any], 
  private val fl : Vector[Option[Any]],
  private val idCv : IdConverter
) extends ImmutableState(st, fl, idCv) with StableState {
  def startStatesUpd() : ImmutableUpdateStates = 
    new ImmutableUpdateStates(st, st, fl, idCv)
}

/** State update assignement vector implementation */
class ImmutableUpdateStates(
  private val stWrite : Vector[Any], 
  private val stRead : Vector[Any], 
  private val fl : Vector[Option[Any]],
  private val idCv : IdConverter
) extends ImmutableState(stRead, fl, idCv) with UpdateStates {
  def writeSV[T](s: Expr.Svar[T], v: T): ImmutableUpdateStates = 
    new ImmutableUpdateStates(stWrite.updated(idCv.stateIntIds(s.uid), v), stRead, fl, idCv)
  def startFlowsUpd(): ImmutableUpdateFlows = 
    new ImmutableUpdateFlows(stWrite, Vector.fill(fl.length)(None), idCv)
}

/** Flow update assignement vector implementation */
class ImmutableUpdateFlows(
  private val st : Vector[Any], 
  private val fl : Vector[Option[Any]],
  private val idCv : IdConverter
) extends ImmutableState(st, fl, idCv) with UpdateFlows {
  def writeFV[T](f: Expr.Fvar[T], v: T): ImmutableUpdateFlows = 
    new ImmutableUpdateFlows(st, fl.updated(idCv.flowIntIds(f.uid), Some(v)), idCv)
  def finishUpd(): ImmutableStableState = 
    new ImmutableStableState(st, fl, idCv)
}

/** @param m flattened model
  * @return variable assignment in intial state */
def immutableInitialState(m : CausalModel) : StableState = {
  val res = 
    new ImmutableUpdateStates(
      Vector.fill(m.idConverter.stateNumber)(-1), 
      Vector.fill(m.idConverter.stateNumber)(-1), 
      Vector.fill(m.idConverter.flowNumber)(None),
      m.idConverter
    )
  val updateState = m.allStateVars.foldLeft(res){case (res, sv@Expr.Svar(uid, init, name, line)) => 
    res.writeSV(sv, init)
  }
  varAssign(updateState.startFlowsUpd(), m.flowDef)
}