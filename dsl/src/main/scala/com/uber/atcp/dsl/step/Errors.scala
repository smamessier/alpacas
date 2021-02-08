package com.uber.atcp.dsl.step

import com.uber.atcp.dsl.errors._

sealed trait StepError extends AlpacasError

case class UnspecifiedExpectation(evname : String) extends StepError {
  override def toString = 
    s"Conflict on event $evname with unspecified weight"
}

case class DivergingUrgentUnfolding(startingPoint : String) extends StepError
