package com.uber.atcp.dsl.flattening

import org.scalacheck._
import org.scalacheck.Prop._

import cats.data.Validated._
import cats.data._
import scala.language.implicitConversions

import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.errors._

val genId = Gen.frequency(
  (4, 1),
  (1, 2),
  (1, 3),
  (1, 4),
  (1, 5)
)

val genPairId = for {
  id1 <- genId
  id2 <- genId
  if id1 != id2
} yield (id1, id2)

class StochasticModel(id1 : Int, id2 : Int) extends Component {
  val ev1 = id1 match
    case 1 => Event()
    case 2 => Event(InputDistribution.Exponential(1E-5))
    case 3 => Event(InputDistribution.Dirac(0), 4)
    case 4 => Event(InputDistribution.Exponential(0.001), Policy.Memory)
    case 5 => Event(InputDistribution.Dirac(100), 5, Policy.Memory)
  val ev2 = id2 match
    case 1 => Event()
    case 2 => Event(InputDistribution.Exponential(1E-5))
    case 3 => Event(InputDistribution.Dirac(0), 4)
    case 4 => Event(InputDistribution.Exponential(0.001), Policy.Memory)
    case 5 => Event(InputDistribution.Dirac(100), 5, Policy.Memory)
  transitions{
    When(ev1) Then{}
    Sync(ev2) With{ev1.soft}
  }
}

def checkComp(c : Component, id1 : Int, id2 : Int) : Boolean = {
  stochasticCheck(c) match
    case Right(cm) => 
      (id1 != 1) && (id2 != 1)
    case Left(err) =>
      err.map{
        case UnspecifiedDistribution("ev1") => id1 == 1
        case UnspecifiedDistribution("ev2") => id2 == 1
        case _ => false
      }.reduce(_&&_) 
}

/** We check that stochastic checks yield errors when events have no distribution and a StochasticModel otherwise. 
  * Tests are performed on a model with two events constructed with a varying pattern. */
object StochasticCheckSpec extends Properties("Stochastic Checks") {
  property("stochastic check") = forAllNoShrink(genPairId) { case (id1, id2) =>
    val m = StochasticModel(id1, id2)
    checkComp(m, id1, id2)
  }
}
