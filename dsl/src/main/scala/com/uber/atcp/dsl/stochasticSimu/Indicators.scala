package com.uber.atcp.dsl.stochasticSimu

import scala.collection.parallel.CollectionConverters._
import cats.{Applicative, Traverse}
import cats.data.Validated._
import cats.implicits._
import scala.language.implicitConversions

import com.uber.atcp.dsl._
import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.frontend._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.state._
import com.uber.atcp.dsl.step._
import com.uber.atcp.dsl.exprEval._
import com.uber.atcp.dsl.flattening._
import com.uber.atcp.dsl.errors._


trait IndicatorsSimu {
    
  /** Stochastic simulation for Mean Time To Failure estimation
    * Potential errors are printed and stop simulation 
    * @param m top-level component
    * @param ndSimulations number of simulations to perform
    * @param condition failure condition
    * @return average MTBF on simulations
    */
  def mttf(
    m : StochasticModel, 
    condition : Expr[Boolean], 
    nbSimulations : Int, 
    batchNumber : Int
  ): EitherResult[Double] = {
    def observer(
      lastTime : Double, 
      state : StableState, 
      deadlock : Boolean, 
      timeState : Double, 
      totalTime : Double
    ) : (Double, Boolean) = {
      (timeState, !eval(condition, state))
    }
    def simulationAggregator(resList : List[Double], lastTime : Double, totalTime : Double) : List[Double] = {
      (totalTime - lastTime)::resList // substract time in failed state
    }
    val batchSize = nbSimulations / batchNumber
    val simus = 
      Vector.fill(batchNumber)(0.0).par.map{i => 
        stochasticSimulationBatch(m, observer, simulationAggregator, batchSize, 0.0, Nil)
      }
    val resSimus = simus.toVector.traverse(identity)
    resSimus.flatMap{ resListList =>
      val resAverages = resListList.map(resList => resList.reduce(_+_) / batchSize)
      Right(resAverages.reduce(_+_) / batchNumber)
    }
  }

  // Not really availability as formal definition, the integral of it
  /** Stochastic simulation for average unavailability
    * Potential errors are printed and stop simulation 
    * @param m top-level component
    * @param duration simulation duration
    * @param condition failure condition
    * @return average MTBF on simulations
    */
  def unavailability(
    m : StochasticModel, 
    condition : Expr[Boolean], 
    duration : Double, 
    nbSimulations : Int
  ) : EitherResult[Double] = {
    def observer(
      time : Double, 
      state : StableState, 
      deadlock : Boolean, 
      timeState : Double, 
      totalTime : Double
    ) : (Double, Boolean) = {
      val addWrong = if (eval(condition, state)) then 0.0 else timeState
      (time + addWrong, totalTime > duration)  
    }
    val simus = Vector.fill(nbSimulations)(0.0).par.map(i => stochasticSimulation(m, observer, 0.0))
    val resSimus : EitherResult[Vector[(Double, Double)]] = simus.toVector.traverse(identity)
    resSimus.flatMap{resList => 
      val average = resList.map((timeFailed, totalTime) => timeFailed / totalTime).reduce(_+_) / nbSimulations
      Right(average)
    }
  }

  /** Stochastic simulation for average unreliability
    * Potential errors are printed and stop simulation 
    * @param m top-level component
    * @param condition failure condition
    * @param duration simulation duration
    * @param nbSimulations number of simulations
    * @param batchNumber number of batches of simulations (executed in parallel)
    * @return average unreliability on simulations
    */
  def unreliability(
    m : StochasticModel, 
    condition : Expr[Boolean], 
    duration : Double, 
    nbSimulations : Int, 
    batchNumber : Int
  ) : EitherResult[Double] = {
    def observer(
      res : Double, 
      state : StableState, 
      deadlock : Boolean, 
      timeState : Double, 
      totalTime : Double
    ) : (Double, Boolean) = {
      val condValue = eval(condition, state)
      (if condValue then 0.0 else 1.0, !condValue || totalTime > duration)  
    }
    def simulationAggregator(resList : List[Double], resSimu : Double, timeSimu : Double) : List[Double] = {
      resSimu::resList
    }
    val batchSize = nbSimulations / batchNumber
    val resSimus = Vector.fill(batchNumber)(0.0).par.map{i => 
      stochasticSimulationBatch(m, observer, simulationAggregator, batchSize, 0.0, Nil)
    }.toVector.traverse(identity)
    resSimus.flatMap{resVect =>
      val resAverage = resVect.map(resList => resList.reduce(_+_) / batchSize)
      Right(resAverage.reduce(_+_) / batchNumber)
    }
  }

}
