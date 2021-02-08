package com.uber.atcp.dsl.assertion

import scala.collection.mutable.ArrayBuffer

import com.uber.atcp.dsl.ast._

type FlowAssertions = ArrayBuffer[FlowAssertion[_]]
case class FlowAssertionBuilder(assertions : FlowAssertions)

/** Flow assertion representation
  */
case class FlowAssertion[T](l : Expr.Fvar[T], r : Expr[T], line : Int) {
  override def toString = s"Assert(\n${l.toString(1)},\n${r.toString(1)})"
}