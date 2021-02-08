package com.uber.atcp.dsl.step

import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._
import com.uber.atcp.dsl.stateImpl._

import scala.language.implicitConversions
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Assertions._


class Engine extends Component {
  val powered = InFlow[Boolean]
  val failed = State[Boolean](false)
  val instruction = InFlow[Boolean]
  val thrust = OutFlow[Boolean]

  val failure = Event()

  assertions{
    thrust := powered && !failed && instruction
  }
  transitions{
    When(failure) If !failed Then {failed := true}
  }
}

class Battery extends Component {
  val power = OutFlow[Boolean]
  val failed = State[Boolean](false)

  val failure = Event()

  assertions{
    power := !failed
  }
  transitions{
    When(failure) If !failed Then {failed := true}
  }
}

class Sensor extends Component {
  val sensedThrust = InFlow[Boolean]
  val transmitted = OutFlow[Boolean]
  val failed = State[Boolean](false)

  val failure = Event()

  assertions{
    transmitted := !failed && sensedThrust
  }
  transitions{
    When(failure) If !failed Then {failed := true}
  }
}

class Function extends Component {
  val sensedThrust = InFlow[Boolean]
  val instruction = OutFlow[Boolean]
  assertions{
    instruction := sensedThrust
  }
}

class FeedbackComp extends Component {
  val engine = Sub(Engine())
  val sensor = Sub(Sensor())
  val function = Sub(Function())
  val battery = Sub(Battery())
  val delay = Sub(Delay[Boolean](true))

  assertions{
    engine.powered := battery.power
    sensor.sensedThrust := engine.thrust
    function.sensedThrust := sensor.transmitted
    engine.instruction := delay.out
    delay.in := function.instruction
  }
}

class DivergingFeedback extends Component {
  val d = Sub(Delay[Boolean](true))

  assertions{
    d.in := !d.out
  }
}

/** Testing that the computed next state corresponds to the expected behaviour in the presence of urgent events. 
  * If a unique stable state can be reached it should be found and returned
  * if several stable states exist, an error should be returned
  * if the it is impossible to converge to a stable state, an error should be returned */
class TestUrgent extends AnyFlatSpec {
  val feedback = FeedbackComp()

  "Feedback component" should "converge" in {
    compCheck(feedback) match 
      case Right(model) =>
        val initialState = immutableInitialState(model)
        val fireableUrgent = EventSet(model)
        fire(model, initialState, feedback.battery.failure.id, fireableUrgent) match
          case Right(state) =>
            assert(true)
          case Left(e) =>
            assert(false, s"Error while firing : $e")
      case Left(e) =>
        assert(false, s"Error while flattening : $e")
  }

  val diverging = DivergingFeedback()

  "Diverging component" should "diverge" in {
    compCheck(diverging) match
      case Right(model) =>
        val fireableUrgent = EventSet(model)
        propagateUrgent(model, immutableInitialState(model), fireableUrgent) match
          case Right(state) =>
            assert(false, s"Converged to state ${state.toString(model)}")
          case Left(e) =>
            assert(true)
      case Left(e) =>
        assert(false, e)

  }
}
