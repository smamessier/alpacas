package com.uber.atcp.dsl.stochasticSimu

import com.uber.atcp.dsl.errors._

sealed trait StochasticSimulationError extends AlpacasError

case class LiveLock() extends StochasticSimulationError 
