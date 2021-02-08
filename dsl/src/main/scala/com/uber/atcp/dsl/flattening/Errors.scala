package com.uber.atcp.dsl.flattening

import com.uber.atcp.dsl.errors._

sealed trait FlatteningError extends AlpacasError

case class MultipleDefinitions(name : String, lines : List[Int]) extends FlatteningError {
  override def toString = 
    s"Variable $name assigned multiple times ${lines.mkString("(lines ", ", ", ")")}"
}
case class NotDefined(name : String, line : Int) extends FlatteningError {
  override def toString = 
    s"Variable $name declared on line $line is not assigned"
}
case class CyclicDefinition(names : List[String]) extends FlatteningError {
  override def toString = 
    s"The assignations of flow variables ${names.mkString(", ")} are cyclic"
}
case class SelfDependent(name : String) extends FlatteningError {
  override def toString = 
    s"The assignation of flow variable $name is self dependent"
}
case class UnspecifiedDistribution(evname : String) extends FlatteningError {
  override def toString = 
    s"The Distribution of event $evname must be specified"
}
case class UnspecifiedError() extends FlatteningError 
case class UUIDconflict() extends FlatteningError 
case class UndefinedFVRead() extends FlatteningError 
