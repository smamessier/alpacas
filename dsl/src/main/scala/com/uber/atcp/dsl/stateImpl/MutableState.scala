package com.uber.atcp.dsl.stateImpl

import scala.collection.mutable.BitSet

import com.uber.atcp.dsl.ast.Expr
import com.uber.atcp.dsl.ast.FlowIds._
import com.uber.atcp.dsl.ast.StateIds._
import com.uber.atcp.dsl.state._
import com.uber.atcp.dsl.step._
import com.uber.atcp.dsl.CausalModel
import com.uber.atcp.dsl.causalModel._

/** Variable assignement array implementation */
class MutableState(
  private val currentSt : Array[Any], 
  private val futureSt : Array[Any], 
  private val fl : Array[Option[Any]],
  private val updated : BitSet,
  private val idCv : IdConverter
) extends State {
  def readSV[T](s : Expr.Svar[T]) : T = currentSt(idCv.stateIntIds(s.uid)).asInstanceOf[T]
  def readFV[T](f : Expr.Fvar[T]) : Option[T] = fl(idCv.flowIntIds(f.uid)).flatMap(t => Some(t.asInstanceOf[T]))
}

/** Read-only assignement array implementation */
class MutableStableState(
  private val currentSt : Array[Any], 
  private val futureSt : Array[Any], 
  private val fl : Array[Option[Any]],
  private val updated : BitSet,
  private val idCv : IdConverter
) extends MutableState(currentSt, futureSt, fl, updated, idCv) with StableState {
  def startStatesUpd() : MutableUpdateStates = new MutableUpdateStates(currentSt, futureSt, fl, updated, idCv)
}

/** State update assignement array implementation */
class MutableUpdateStates(
  private val currentSt : Array[Any], 
  private val futureSt : Array[Any], 
  private val fl : Array[Option[Any]],
  private val updated : BitSet,
  private val idCv : IdConverter
) extends MutableState(currentSt, futureSt, fl, updated, idCv) with UpdateStates {
  def writeSV[T](s : Expr.Svar[T], v : T) : MutableUpdateStates = {
    futureSt(idCv.stateIntIds(s.uid)) = v
    updated.add(idCv.stateIntIds(s.uid))
    this
  }
  def startFlowsUpd() : MutableUpdateFlows = {
    updated.foreach{n => currentSt(n) = futureSt(n)}
    updated.clear()
    for (i <- 0 until fl.length){
      fl(i) = None
    }
    new MutableUpdateFlows(currentSt, futureSt, fl, updated, idCv)
  } 
}

/** Flow update assignement array implementation */
class MutableUpdateFlows(
  private val currentSt : Array[Any], 
  private val futureSt : Array[Any], 
  private val fl : Array[Option[Any]],
  private val updated : BitSet,
  private val idCv : IdConverter
) extends MutableState(currentSt, futureSt, fl, updated, idCv) with UpdateFlows {
  def writeFV[T](f : Expr.Fvar[T], v : T) : MutableUpdateFlows = {
    fl(idCv.flowIntIds(f.uid)) = Some(v)
    this
  }
  def finishUpd() : MutableStableState = new MutableStableState(currentSt, futureSt, fl, updated, idCv)
}

/** @param m flattened model
  * @return variable assignment in intial state */
def mutableInitialState(model : CausalModel) : StableState = {
  val res = new MutableUpdateStates(
    Array.fill(model.idConverter.stateNumber)(-1), 
    Array.fill(model.idConverter.stateNumber)(-1), 
    Array.fill(model.idConverter.flowNumber)(None),
    new BitSet(model.idConverter.stateNumber),
    model.idConverter
  )
  val updateState = model.allStateVars.foldLeft(res){case (res, sv@Expr.Svar(uid, init, name, line)) => res.writeSV(sv, init)}
  varAssign(updateState.startFlowsUpd(), model.flowDef)
}
