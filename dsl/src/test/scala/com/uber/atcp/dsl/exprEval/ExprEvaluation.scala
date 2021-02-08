package com.uber.atcp.dsl.exprEval

import org.scalacheck._
import org.scalacheck.Prop.forAll
import Gen._
import scala.math.{max, min}


import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl._
import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.step._
import com.uber.atcp.dsl.stateImpl._
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl.ast.Expr

import scala.language.implicitConversions

class TestComponent extends Component {

  val stateInt = State[Int](0)
  val stateBool = State[Boolean](true)

  val outFlowInt = OutFlow[Int]
  val outFlowBool = OutFlow[Boolean]

  assertions{
    outFlowInt := stateInt
    outFlowBool := stateBool
  }

}

/** Property-based testing for expressions evaluation: 
  * We test that for every expression construct (boolean operations, numeric operation, comparisons, …), 
  * if we build an expression with constant subexpressions (of generated values) and evaluate it, 
  * the result corresponds to the semantics of the construct on the corresponding values.
  */
object ExprEvaluationSpec extends Properties("Expression evaluation") {

  val testComp = TestComponent()

  val causalModel : CausalModel = 
    compCheck(testComp) match
      case Right(cm) => cm
      case Left(err) => null
  
  val state = mutableInitialState(causalModel)

  enum FailureMode derives Lifted {
    case Ok
    case Err
    case Lost
  }

  import FailureMode._

  given Ord[FailureMode] {
    def lt(x : FailureMode, y : FailureMode) : Boolean = 
      (x == Ok && y != Ok) || (x == Err && y == Lost)  
  }

  given Arbitrary[FailureMode] = Arbitrary(oneOf(FailureMode.Ok, FailureMode.Err, FailureMode.Lost))

  property("additionInt") = forAll {(a : Int, b : Int) =>
    val expr1 : Expr[Int] = Expr.Const(a) + b
    (eval(expr1, state) == a + b)
  }

  property("substractionInt") = forAll {(a : Int, b : Int) =>
    val expr1 : Expr[Int] = Expr.Const(a) - b
    (eval(expr1, state) == a - b)
  }

  property("multiplicationInt") = forAll {(a : Int, b : Int) =>
    val expr1 : Expr[Int] = Expr.Const(a) * b
    (eval(expr1, state) == a * b)
  }

  val nonZeroInteger = Arbitrary.arbitrary[Int] suchThat (_ != 0)

  property("divisionInt") = forAll(Gen.zip(Arbitrary.arbitrary[Int], nonZeroInteger)) {(a, b) =>
    val expr1 : Expr[Int] = Expr.Const(a) / b
    (eval(expr1, state) == a / b)
  }

  property("leftAdditionInt") = forAll {(a : Int, b : Int) =>
    val expr1 : Expr[Int] = a + Expr.Const(b)
    (eval(expr1, state) == a + b)
  }

  property("leftSubstractionInt") = forAll {(a : Int, b : Int) =>
    val expr1 : Expr[Int] = a - Expr.Const(b)
    (eval(expr1, state) == a - b)
  }

  property("leftMultiplicationInt") = forAll {(a : Int, b : Int) =>
    val expr1 : Expr[Int] = a * Expr.Const(b)
    (eval(expr1, state) == a * b)
  }

  property("leftDivisionInt") = forAll(Gen.zip(Arbitrary.arbitrary[Int], nonZeroInteger)) {(a, b) =>
    val expr1 : Expr[Int] = a / Expr.Const(b)
    (eval(expr1, state) == a / b)
  }
  
  property("additionDouble") = forAll {(a : Double, b : Double) =>
    val expr1 : Expr[Double] = Expr.Const(a) + b
    (eval(expr1, state) == a + b)
  }

  property("substractionDouble") = forAll {(a : Double, b : Double) =>
    val expr1 : Expr[Double] = Expr.Const(a) - b
    (eval(expr1, state) == a - b)
  }

  property("multiplicationDouble") = forAll {(a : Double, b : Double) =>
    val expr1 : Expr[Double] = Expr.Const(a) * b
    (eval(expr1, state) == a * b)
  }

  val nonZeroDouble = Arbitrary.arbitrary[Double] suchThat (_ != 0.0)

  property("divisionDouble") = forAll(Gen.zip(Arbitrary.arbitrary[Double], nonZeroDouble)) {(a, b) =>
    val expr1 : Expr[Double] = Expr.Const(a) / b
    (eval(expr1, state) == a / b)
  }

  property("conjunction") = forAll {(a : Boolean, b : Boolean) =>
    val expr1 : Expr[Boolean] = Expr.Const(a) && b
    (eval(expr1, state) == (a && b))
  }

  property("disjunction") = forAll {(a : Boolean, b : Boolean) =>
    val expr1 : Expr[Boolean] = Expr.Const(a) || b
    (eval(expr1, state) == (a || b))
  }

  property("negation") = forAll {(a : Boolean) =>
    val expr1 : Expr[Boolean] = !Expr.Const(a)
    (eval(expr1, state) == !a)
  }

  property("leftConjunction") = forAll {(a : Boolean, b : Boolean) =>
    val expr1 : Expr[Boolean] = a && Expr.Const(b)
    (eval(expr1, state) == (a && b))
  }

  property("leftDisjunction") = forAll {(a : Boolean, b : Boolean) =>
    val expr1 : Expr[Boolean] = a || Expr.Const(b)
    (eval(expr1, state) == (a || b))
  }

  property("equality") = forAll {(a : FailureMode, b : FailureMode) =>
    val expr1 : Expr[Boolean] = Expr.Const(a) === b
    (eval(expr1, state) == (a == b))
  }

  property("lt") = forAll{(a : Int, b : Int) =>
    val expr1 : Expr[Boolean] = Expr.Const(a) < b
    (eval(expr1, state) == (a < b))
  }

  property("leq") = forAll{(a : Int, b : Int) =>
    val expr1 : Expr[Boolean] = Expr.Const(a) <= b
    (eval(expr1, state) == (a <= b))
  }

  property("gt") = forAll{(a : Int, b : Int) =>
    val expr1 : Expr[Boolean] = Expr.Const(a) > b
    (eval(expr1, state) == (a > b))
  }

  property("geq") = forAll{(a : Int, b : Int) =>
    val expr1 : Expr[Boolean] = Expr.Const(a) >= b
    (eval(expr1, state) == (a >= b))
  }

  property("max") = forAll{(a : Int, b : Int) =>
    val expr1 : Expr[Int] = Expr.Const(a) max b
    (eval(expr1, state) == (a max b))
  }

  property("min") = forAll{(a : Int, b : Int) =>
    val expr1 : Expr[Int] = Expr.Const(a) min b
    (eval(expr1, state) == (a min b))
  }

  property("leftLt") = forAll{(a : Double, b : Double) =>
    val expr1 : Expr[Boolean] = a < Expr.Const(b) 
    (eval(expr1, state) == (a < b))
  }

  property("leftleq") = forAll{(a : Double, b : Double) =>
    val expr1 : Expr[Boolean] = a <= Expr.Const(b) 
    (eval(expr1, state) == (a <= b))
  }

  property("leftGt") = forAll{(a : Double, b : Double) =>
    val expr1 : Expr[Boolean] = a > Expr.Const(b) 
    (eval(expr1, state) == (a > b))
  }

  property("leftGeq") = forAll{(a : Double, b : Double) =>
    val expr1 : Expr[Boolean] = a >= Expr.Const(b) 
    (eval(expr1, state) == (a >= b))
  }

  property("leftMax") = forAll{(a : Double, b : Double) =>
    val expr1 : Expr[Double] = a max Expr.Const(b) 
    (eval(expr1, state) == (a max b))
  }

  property("leftMin") = forAll{(a : Double, b : Double) =>
    val expr1 : Expr[Double] = a min Expr.Const(b) 
    (eval(expr1, state) == (a min b))
  }

  property("LiftedOrd") = forAll{(a : FailureMode, b : FailureMode) => 
    val expr = Expr.Const(a) < b
    eval(expr, state) == (a < b)
  }

  property("LeftLiftedOrdLt") = forAll{(a : FailureMode, b : FailureMode) => 
    val expr = a < Expr.Const(b)
    eval(expr, state) == (a < b)
  }

  property("LeftLiftedOrdLeq") = forAll{(a : FailureMode, b : FailureMode) => 
    val expr = a <= Expr.Const(b)
    eval(expr, state) == (a <= b)
  }

  property("LeftLiftedOrdGt") = forAll{(a : FailureMode, b : FailureMode) => 
    val expr = a > Expr.Const(b)
    eval(expr, state) == (a > b)
  }

  property("LeftLiftedOrdGeq") = forAll{(a : FailureMode, b : FailureMode) => 
    val expr = a >= Expr.Const(b)
    eval(expr, state) == (a >= b)
  }

  property("ifThenElse") = forAll{(c : Boolean, t : Int, f : Int) =>
    val expr : Expr[Int] = If (c) Then t Else f
    (eval(expr, state) == (if c then t else f))
  }

  property("ElseIf") = forAll{(c1 : Boolean, v1 : Int, c2 : Boolean, v2 : Int, v3 : Int) =>
    val expr : Expr[Int] = If (c1) Then v1 ElseIf(c2) Then v2 Else v3
    (eval(expr, state) == (if c1 then v1 else if c2 then v2 else v3))
  }

  property("DoubleElseIf") = forAll{(
      c1 : Boolean, 
      v1 : Int, 
      c2 : Boolean, 
      v2 : Int, 
      c3 : Boolean, 
      v3 : Int, 
      v4 : Int
    ) =>
      val expr : Expr[Int] = If (c1) Then v1 ElseIf(c2) Then v2 ElseIf(c3) Then v3 Else v4
      (eval(expr, state) == (if c1 then v1 else if c2 then v2 else if c3 then v3 else v4))
  }

  property("Switch") = forAll{(switched : Int, t1 : Int, t2 : Int, v1 : Int, v2 : Int, d : Int) =>
    val expr : Expr[Int] = 
      Switch(switched){
        t1 -> v1
        t2 -> v2
        Default(d)
      }
    val expected = if switched == t1 then v1 else if switched == t2 then v2 else d
    (eval(expr, state) == expected)
  }

}