package com.uber.atcp.dsl.step

import scala.collection.mutable.ArrayBuffer
import org.scalacheck._
import org.scalacheck.Prop.forAll
import Arbitrary.arbitrary
import Gen._

import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.flattening._
import com.uber.atcp.dsl.stateImpl._
import com.uber.atcp.dsl.exprEval._
import com.uber.atcp.dsl.frontend._
import com.uber.atcp.dsl._
import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._

class FireableModel extends Component {
  val sInt1 = State[Int](0)
  val sInt2 = State[Int](1)
  val sBool = State[Boolean](true)

  val e1 = Event()
  val e2 = Event()
}

class HiddenModel extends Component {
  val e = Event()
  transitions{
    When(e) Then {}
  }
}

val c = FireableModel()

val genLeafBool : Gen[Expr[Boolean]] = oneOf(const(c.sBool), arbitrary[Boolean].flatMap(Expr.Const(_)))

val genLeafInt : Gen[Expr[Int]] = oneOf(oneOf(c.sInt1, c.sInt2), arbitrary[Int].flatMap(Expr.Const(_)))

val exprGen =  ExprGenerator(genLeafBool, genLeafInt, false)

given Arbitrary[Expr[Boolean]] = Arbitrary(exprGen.genExprBool)

/** Fireables transitions update property based testing :
  * We check that the enumeration of fireable transitions corresponds to those for which 
  * the guard evaluates to true. We verify that hidden transitions are never fireable. */
object ExprEvaluationSpec extends Properties("Update fireable") {
  property("Update Fireable") = forAll{ (g1 : Expr[Boolean], g2 : Expr[Boolean], mutable : Boolean) =>
    given Transitions = c.trans.transitions
    When(c.e1) If(g1) Then {}
    When(c.e2) If(g2) Then {}

    compCheck(c) match
      case Right(cm) =>
        val state = 
          if mutable
            mutableInitialState(cm)
          else
            immutableInitialState(cm)

        val fireablesRef = for{
          (g, id) <- Set((g1, c.e1.id), (g2, c.e2.id))
          if(eval(g, state))
        } yield id

        val fireables = EventSet(cm)
        updateFireable(cm, state, fireables)

        val fireablesTest = fireables.toVector.toSet

        c.trans.transitions -= c.trans.transitions.last
        c.trans.transitions -= c.trans.transitions.last

        fireablesTest == fireablesRef
      case Left(err) =>
        false
  }

  property("hidden not fireable") = forAll{(h : Boolean, mutable : Boolean) =>
    val m = HiddenModel()
    if h
      m.e.hide
    compCheck(m) match
      case Right(cm) =>
        val state = 
          if mutable
            mutableInitialState(cm)
          else
            immutableInitialState(cm)

        val fireables = EventSet(cm)
        updateFireable(cm, state, fireables)

        if h
          fireables.size == 0
        else 
          fireables.size == 1 && fireables.contains(m.e.id)

      case Left(err) => false
  }
}