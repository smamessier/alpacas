package com.uber.atcp.dsl.frontend

import com.uber.atcp.dsl.errors._

sealed trait FrontendError extends AlpacasError

case class VectorAssignementSizeError(line : Int) extends FrontendError {
  override def toString : String = 
    s"Different sizes in vector assignement on line $line"
}
