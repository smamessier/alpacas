package com.uber.atcp.dsl.frontend

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Assertions._
import scala.language.implicitConversions

import com.uber.atcp.dsl.assertion._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl._
import com.uber.atcp.dsl.Modelling.{_, given _}

/** Testing that the object of the class Component representing the model corresponds to the declarations. 
  * We verify on adapted models the bookkeeping for declarations of 
  * flows, states and subcomponents (including in vectors), assertions and transitions.
  * We also verify that declaration of assertions and transitions outside of the corresponding blocks yield a type error. 
  */
class ComponentDeclarationSpec extends AnyFlatSpec {

  "flow variables" should "be registered in fvar object" in {
    class FlowComp extends Component {
      val v1 = OutFlow[Int]
      val v2 = InFlow[Boolean]
      val v3 = OutFlow[Int]
    }

    val c = FlowComp()

    assert(c.fvar.out.size == 2)

    assert(c.fvar.in.size == 1)
    assert(c.fvar.in contains c.v2)
    assert(c.fvar.out contains c.v1)
    assert(c.fvar.out contains c.v3)
  }

  "flow variables vectors" should "be registered in fvar" in {
    class FlowVectComp extends Component {
      val v1 = InFlows[Int](3)
      val v2 = OutFlows[Boolean](4)
    }

    val c = FlowVectComp()

    assert(c.fvar.out.size == 4)
    assert(c.fvar.in.size == 3)
    assert(c.v1 forall {v => c.fvar.in contains v})
    assert(c.v2 forall {v => c.fvar.out contains v})
  }

  "state variables" should "be registered in svar" in {
    class StateComp extends Component {
      val v1 = State[Int](init = 3)
      val v2 = State[Boolean](init = false)
      val v3 = State[Int](2)
    }

    val c = StateComp()

    assert(c.svar.size == 3)
    assert(c.svar contains c.v2)
    assert(c.svar contains c.v1)
    assert(c.svar contains c.v3)
    assert(c.v1.init == 3)
    assert(c.v2.init == false)
    assert(c.v3.init == 2)
  }

  "state variables vectors" should "be registered in svar" in {
    class StateVectComp extends Component {
      val v1 = States[Int](3)(i => i)
      val v2 = States[Boolean](4)(false)
    }

    val c = StateVectComp()

    assert(c.svar.size == 7)
    assert(c.v1 forall {v => c.svar contains v})
    assert(c.v2 forall {v => c.svar contains v})
    assert(c.v1.zipWithIndex forall {case (v, i) => v.init == i})
    assert(c.v2 forall {v => v.init == false})
  }

  "subcomponents" should "be registered in subcomps" in {
    class TopComp extends Component {
      val sub = Sub(SubComp())
    }

    class SubComp extends Component {
      val subsub = Sub(SubSubComp())
    }

    class SubSubComp extends Component

    val c = TopComp()

    assert(c.subcomps.size == 1)
    assert(c.subcomps contains c.sub)
    assert(c.sub.subcomps.size == 1)
    assert(c.sub.subcomps contains c.sub.subsub)
    assert(c.sub.subsub.subcomps.size == 0)
  }

  "subcomponents vector" should "be registered in subcomps" in {
    class TopComp extends Component {
      val sub = Subs(3)(SubComp(2))
      val subIndexed = Subs(4)(i => SubComp(i))
    }

    class SubComp(i : Int) extends Component {
      val s = States[Boolean](i)(false)
    }

    val c = TopComp()

    assert(c.subcomps.size == 7)
    assert(c.sub forall {s => c.subcomps contains s})
    assert(c.subIndexed forall {s => c.subcomps contains s})
    assert(c.subIndexed.zipWithIndex forall {case (s, i) => s.svar.size == i})
  }

  "assertions" should "be registered in assert.assertions" in {
    class AssertionComp extends Component {
      val v1 = OutFlow[Int]
      val v2 = InFlow[Int]
      val v3 = OutFlow[Boolean]

      assertions{
        v1 := v2
        v3 := false
      }
    }

    val c = AssertionComp()

    assert(c.assert.assertions.size == 2)
    assert(c.assert.assertions exists {
      case FlowAssertion(c.v1, c.v2, _) => true
      case _ => false 
    })
    assert(c.assert.assertions exists {
      case FlowAssertion(c.v3, Expr.Const(false), _) => true
      case _ => false 
    })
  }

  "vector assertions" should "be registered in assert.assertions" in {
    class AssertionComp extends Component {
      val v1 = OutFlows[Int](5)
      val v2 = InFlows[Int](5)

      assertions{
        v1 := v2
      }
    }

    val c = AssertionComp()

    assert(c.assert.assertions.size == 5)
    assert(c.v1 zip c.v2 forall {case (out, in) =>
      c.assert.assertions exists {case FlowAssertion(l, r, _) => 
        l == out && r == in
      }
    })
  }

  "vector assertions for different sizes" should "raise an exception" in {
    class AssertionComp extends Component {
      val v1 = OutFlows[Int](5)
      val v2 = InFlows[Int](6)

      assertions{
        v1 := v2
      }
    }

    try{
      val c = AssertionComp()
      assert(false, "no exception on different size vector assertion")
    } catch {
      case e : VectorAssignementSizeError => assert(true)
    }
  }

  "assertion out of block" should "not typecheck" in {
    val wrongDecl =
      """
        |class Wrong extends Component{
        |  val v1 = InFlow[Boolean]
        |  val v2 = OutFlow[Boolean]
        |  v1 := v2
        |}
        |""".stripMargin
    assertDoesNotCompile(wrongDecl)
  }

  "transitions" should "be registered in trans.transitions" in {
    class TransComp extends Component {
      val v1 = State[Int](0)
      val v2 = State[Boolean](false)
      val e1 = Event()
      val e2 = Event(InputDistribution.Exponential(1E-5))
      val e3 = Event(InputDistribution.Dirac(0), 4)
      val e4 = Event(InputDistribution.Exponential(0.001), Policy.Memory)
      val e5 = Event(InputDistribution.Dirac(100), 5, Policy.Memory)

      transitions{
        When(e1) If !v2 Then {v2 := true}
        When(e2) Then {v1 := v1 + 1}
      }
    }

    val c = TransComp()

    assert(c.trans.transitions.size == 2)
    assert(c.trans.transitions exists {
      case InputTransition(c.e1, Synchronizable.Explicit(Expr.Un(Unop.Neg, c.v2), a)::Nil) => a exists {
        case StateAssertion(c.v2, Expr.Const(true), _) => true
        case _ => false}
      case _ => false}
    )
    assert(c.trans.transitions exists {
      case InputTransition(c.e2, Synchronizable.Explicit(Expr.Const(true), a)::Nil) => a exists {
        case StateAssertion(c.v1, Expr.NumBin(NumBinop.Add, c.v1, Expr.Const(1)), _) => true
        case _ => false}
      case _ => false}
    )
   
    assert(!c.e1.distrib.isDefined)
    assert(!c.e1.weight.isDefined)
    assert(c.e1.policy == Policy.Restart)

    assert(c.e2.distrib match 
      case Some(InternalDistribution.Exponential(_)) => true
      case _ => false)
    assert(!c.e2.weight.isDefined)
    assert(c.e2.policy == Policy.Restart)

    assert(c.e3.distrib  match 
      case Some(InternalDistribution.Dirac(0)) => true
      case _ => false)
    assert(c.e3.weight == Some(4))
    assert(c.e3.policy == Policy.Restart)

    assert(c.e4.distrib match 
      case Some(InternalDistribution.Exponential(_)) => true
      case _ => false)
    assert(!c.e4.weight.isDefined)
    assert(c.e4.policy == Policy.Memory)

    assert(c.e5.distrib match 
      case Some(InternalDistribution.Dirac(100)) => true
      case _ => false)
    assert(c.e5.weight == Some(5))
    assert(c.e5.policy == Policy.Memory)
  }

  "transition out of block" should "not typecheck" in {
    val wrongDecl =
      """
        |class Wrong extends Component{
        |  val v1 = State[Boolean](true)
        |  val e = Event()
        |  When(e) If(v1) Then {v1 := false}
        |}
        |""".stripMargin
    assertDoesNotCompile(wrongDecl)
  }
}
