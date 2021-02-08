package com.uber.atcp.dsl.stochasticSimu

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Assertions._
import scala.language.implicitConversions

import com.uber.atcp.dsl._
import com.uber.atcp.dsl.Modelling._
import com.uber.atcp.dsl.Analysis._


import com.uber.atcp.dsl.mcs.SeriesParallel.{CompSpec, genAllSystems}

val n = 4
val rng = scala.util.Random(seed = 0)
val dists = Vector(
  InputDistribution.Exponential(10.0),
  InputDistribution.Exponential(100.0),
  InputDistribution.Exponential(1e-2),
  InputDistribution.Exponential(1e-3),
  InputDistribution.Exponential(1e-4),
  InputDistribution.Exponential(1e-5)
)

given Ord[Boolean] {
  def lt(x: Boolean, y: Boolean): Boolean = (!x && y)
}
val components = (1 to n).map(CompSpec(_,worstMode, dists(rng.nextInt(dists.size)))).toList
val (bestMode, worstMode) = (true, false)
val systems = genAllSystems(components, bestMode, worstMode)

val durations = List(1e5, 1e7)
val maxRelativeD = 0.1
val nbSimu = 10000

/** Equivalence checking to crossvalidate Reliability estimation by stochastic simulation and exact 
  * computation from MCS */
class TestMCS extends AnyFlatSpec {
  "Stochastic simu reliability estimation" should "be close to the exact value computed from MCS" in {
    systems foreach{system =>
      stochasticCheck(system).fold(
        e => assert(false, "Error in model definition"),
        model =>
          for {
            d <- durations
            mcs <- minimalCutSetsBFS(model, system.obs, n)
            urSimu <- unreliability(model, system.obs, d, nbSimu, 10)
            urMCS = unreliabilityMCS(model, d, mcs)
            relativeD = (urMCS - urSimu).abs / urMCS
          } {
            assert(relativeD <= maxRelativeD, s"Unreliability by simulation : $urSimu and by MCS : $urMCS, duration: $d")
          }
      )  
    }
  }

}
