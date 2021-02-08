package com.uber.atcp.dsl.causalModel

import com.uber.atcp.dsl.Component
import com.uber.atcp.dsl.frontend._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.ast.FlowIds._
import com.uber.atcp.dsl.ast.StateIds._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.transition.EventIds._
import com.uber.atcp.dsl.assertion.FlowAssertion

/** Interface for storage of full names of state and flow variables and events */
class VarNames(
  val stateNames : Array[String],
  val flowNames : Array[String],
  val evNames : Array[String],
  val idConverter : IdConverter,
){
  def getState(s : StateId) : String = stateNames(idConverter.stateIntIds(s))
  def getFlow(f : FlowId)  : String = flowNames(idConverter.flowIntIds(f))
  def getEvent(e : EventId) : String = evNames(idConverter.evIntIds(e))
  def setState(s : StateId, name : String) : Unit = stateNames(idConverter.stateIntIds(s)) = name
  def setFlow(f : FlowId, name : String)  : Unit = flowNames(idConverter.flowIntIds(f)) = name
  def setEvent(e : EventId, name : String) : Unit = evNames(idConverter.evIntIds(e)) = name
}

type FlowDefinition = Vector[FlowAssertion[_]]

/** Structure for conversion of UUIDs to Ints for array storage of flows, tates and events */
class IdConverter(
  val evIntIds : Map[EventId,Int], 
  val stateIntIds : Map[StateId,Int],
  val flowIntIds : Map[FlowId,Int],
  val evNumber : Int, 
  val stateNumber : Int,
  val flowNumber : Int
)
