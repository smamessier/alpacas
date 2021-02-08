package com.uber.atcp.dsl.mcs

import com.uber.atcp.dsl.errors._

sealed trait McsError extends AlpacasError

case class UnspecifiedDistribution(evname : String) extends McsError {
  override def toString = 
    s"The Distribution of event $evname must be specified"
}
