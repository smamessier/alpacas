package com.uber.atcp.dsl.flattening

import scala.math.min
import scala.collection.mutable.HashMap

import com.uber.atcp.dsl.Component
import com.uber.atcp.dsl.frontend._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.transition.EventIds._
import com.uber.atcp.dsl.ast.FlowIds._
import com.uber.atcp.dsl.ast.StateIds._
import com.uber.atcp.dsl.causalModel.VarNames

/** State variables gathering from full model
  * Number of variables count for data structure sizing
  * Maping from state uuids to int ids for indexing
  */
def allStateVars(comp : Component, count : Int, idMap : HashMap[StateId,Int]) : (StateVarSet, Int) = {
  val newCount = comp.svar.foldLeft(count){case (c, v) => 
    if idMap.contains(v.uid)
      throw UUIDconflict()
    idMap += ((v.uid, c))
    c + 1  
  }
  comp.subcomps.foldLeft((comp.svar, newCount)){case ((svSet, c), sub) => 
    val (svSub, newC) = allStateVars(sub, c, idMap)
    (svSet ++ svSub, newC)
  }
}

/** Flow variables gathering from full model
  * Number of variables count for data structure sizing
  * Maping from flow uuids to int ids for indexing
  */
def allFlowVars(comp : Component, count : Int, idMap : HashMap[FlowId,Int]) : (FlowVarSet, Int) = {
  val ownFVars = comp.fvar.in ++ comp.fvar.out
  val newCount = ownFVars.foldLeft(count){case (c, v) =>
    if idMap.contains(v.uid)
      throw UUIDconflict()
    idMap += ((v.uid, c))
    c + 1  
  }
  comp.subcomps.foldLeft((ownFVars, newCount)){case ((fvSet, c), sub) => 
    val (fvSub, newC) = allFlowVars(sub, c, idMap)
    (fvSet ++ fvSub, newC)
  }
}

/** Number of events count for data structure sizing
  * Maping from state uuids to int ids for indexing  */
def eventNumbering(comp : Component, count : Int, idMap : HashMap[EventId,Int]) : Int = { 
  val newCount = comp.trans.transitions.foldLeft(count){case (c, t) => 
    if idMap.contains(t.event.id)
      throw UUIDconflict()
    idMap += ((t.event.id, c))
    c + 1
  }
  comp.subcomps.foldLeft(newCount){case (c, sub) => 
    val newC = eventNumbering(sub, c, idMap)
    newC
  }
}

/** Transitions gathering from full model */
def allTransitions(c : Component, v : Vector[InputTransition], idMap : HashMap[EventId,Int]) : Vector[InputTransition] = {
  val ownTrans = c.trans.transitions.foldLeft(v){case (v, t) => v.updated(idMap(t.event.id), t)}
  c.subcomps.foldLeft(ownTrans){case (allTrans, sub) => allTransitions(sub, allTrans, idMap)}
}

/** Variables and events hierarchical renaming */
def nameVars(c : Component, varNames : VarNames, path : String) : Unit = {
  if (c.name != "") then c.fullName = path + c.name + "."
  c.fvar.in foreach {v => varNames.setFlow(v.uid, c.fullName + v.name)}
  c.fvar.out foreach {v => varNames.setFlow(v.uid, c.fullName + v.name)}
  c.svar foreach {v => varNames.setState(v.uid, c.fullName + v.name)}
  c.trans.transitions foreach {t => varNames.setEvent(t.event.id, c.fullName + t.event.name)}
  c.subcomps foreach {s => nameVars(s, varNames, c.fullName)}
}