package com.uber.atcp.dsl.flattening

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import scala.language.implicitConversions

import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._
import com.uber.atcp.dsl.frontend._
import com.uber.atcp.dsl.stateImpl._
import com.uber.atcp.dsl.step._
import com.uber.atcp.dsl.state._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.causalModel._


/** We check that the flattened synchronisations give the same resulting states when fired as the step semantics 
  * defined by module transitions. 
  * Tests are performed with varying soft / hard combinations of different transitions. */
object SynchrosSpec extends Properties("Synchros") {

  import SynchroTests._

  class Subcomp extends Component {
    val subBool1 = State[Boolean](true)
    val subBool2 = State[Boolean](true)
    val subev1 = Event()
    val subev2 = Event()
    val subSync = Event()

    subev1.hide

    transitions{
      When(subev1) If(subBool1) Then {subBool1 := false}
      When(subev2) If(subBool2) Then {subBool2 := false}
    }
  }

  class SynchroModel extends Component {
    val sBool1 = State[Boolean](true)
    val sBool2 = State[Boolean](true)
    val sInt1 = State[Int](0)
    val sInt2 = State[Int](0)
    val sInt3 = State[Int](0)

    val sub = Sub(Subcomp())

    val evB1 = Event()
    val evB2 = Event()
    val evI1 = Event()
    val evI2 = Event()
    val sync = Event()

    transitions{
      When(evB1) If(sBool1) Then {sBool1 := false}
      When(evB2) If(sBool2) Then {sBool2 := false}
      When(evI1) If(sInt1 === 0) Then {sInt1 := sInt2}
      When(evI2) If(sInt2 < 3) Then {sInt2 := sInt2 + 1}
    }
  }
  
  val m = SynchroModel()

  extension (s : State) 
    def compare(that : State, m : CausalModel) : Boolean = {
      val flowEq = m.allFlowVars.map(v => s.readFV(v) == that.readFV(v))
      val stateEq = m.allStateVars.map(v => s.readSV(v) == that.readSV(v))
      (flowEq++stateEq).foldLeft(true){case (res, b) => res&&b}
    }

  val syncB1 : Gen[Synchro] = oneOf(m.evB1.hard, m.evB1.soft)
  val syncB2 : Gen[Synchro] = oneOf(m.evB2.hard, m.evB2.soft)
  val syncI1 : Gen[Synchro] = oneOf(m.evI1.hard, m.evI1.soft)
  val syncI2 : Gen[Synchro] = oneOf(m.evI2.hard, m.evI2.soft)
  val syncSub : Gen[Synchro] = oneOf(m.sub.subSync.hard, m.sub.subSync.soft)

  val syncTopLevel : Gen[Synchro] = 
    for {
      sb1 <- syncB1
      sb2 <- syncB2
      sI1 <- syncI1
      sI2 <- syncI2
      sSub <- syncSub
    } yield (sb1 & sb2 & sI1 & sI2 & sSub)
  
  val syncSubLevel : Gen[Synchro] = 
    for {
      s1 <- oneOf(m.sub.subev1.hard, m.sub.subev1.soft)
      s2 <- oneOf(m.sub.subev2.hard, m.sub.subev2.soft)
    } yield s1 & s2


  def sameSynchroStep(initial : StableState, cm : CausalModel, m : Component, s : Event) : Boolean = {
    val fireableCoherent = fireableSynchro(initial, s.id, m) == (fireable(cm, initial) contains s)

    if fireableSynchro(initial, s.id, m)
      val stateFlattened = fireOne(cm, initial, s.id)
      val stateUnflattened = synchroStep(initial, s.id, m, cm.flowDef)

      fireableCoherent && stateFlattened.compare(stateUnflattened, cm)
    else 
      fireableCoherent
  }

  val genEvent : Gen[Event] = oneOf(m.evB1, m.evB2, m.evI1, m.evI2)

  property("Synchro") = forAll(
    syncTopLevel, 
    syncSubLevel, 
    arbitrary[Boolean], 
    arbitrary[Int], 
    genEvent, 
    oneOf("anonymous", "strongerGuard", "classic")
  ) { (stl, ssl, g, vI3, e, synchroType) =>

    given Transitions = m.trans.transitions
    synchroType match
      case "anonymous" => Sync(m.sync) If(g) Then{m.sInt3 := vI3} With(stl)
      case "strongerGuard" => Sync(m.sync) If(g) With(stl) 
      case "classic" => Sync(m.sync) With(stl) 
    
    val subScope = {
      given Transitions = m.sub.trans.transitions
      Sync(m.sub.subSync) With(ssl)
    }

    val cm = compCheck(m) match 
      case Right(r) => r
      case Left(err) => throw err.head

    val initial = immutableInitialState(cm)

    val startState = fireOne(cm, initial, e.id)
    
    val res = sameSynchroStep(startState, cm, m, m.sync)
    m.trans.transitions -= m.trans.transitions.last
    m.sub.trans.transitions -= m.sub.trans.transitions.last
    res
  }

  
}