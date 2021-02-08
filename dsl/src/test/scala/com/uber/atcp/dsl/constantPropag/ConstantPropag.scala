package com.uber.atcp.dsl.constantPropag

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import scala.collection.mutable.ArrayBuffer

import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.ast.StateIds._
import com.uber.atcp.dsl.ast.FlowIds._
import com.uber.atcp.dsl.assertion._
import com.uber.atcp.dsl.exprEval._
import com.uber.atcp.dsl.stateImpl._
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._
import com.uber.atcp.dsl.Modelling.{_, given _}

/** Property-based testing on generated expressions: 
  * we check that the evaluation of the raw expression gives the same result as 
  * the evaluation of the simplified expression.
  */
object CstPropagSpec extends Properties("Constant propagation") {
  val sBool = Expr.Svar(StateId(), true, "sbool", 12).asInstanceOf[Expr.Svar[Boolean]]

  val fBool = Expr.Fvar[Boolean](FlowId(), "fbool", 15).asInstanceOf[Expr.Fvar[Boolean]]

  val genLeafBool : Gen[Expr[Boolean]] = frequency((1, sBool), (1, fBool), (10, arbitrary[Boolean].flatMap(Expr.Const(_))))


  val sInt = Expr.Svar(StateId(), 1, "sInt", 50).asInstanceOf[Expr.Svar[Int]]

  val fInt = Expr.Fvar[Int](FlowId(), "fInt", 53).asInstanceOf[Expr.Fvar[Int]]

  val genLeafInt : Gen[Expr[Int]] = 
    frequency(
      (1, sInt), 
      (1, fInt), 
      (10, (arbitrary[Int] suchThat (_ !=0)).flatMap(Expr.Const(_)))
    )

  val genExpr = ExprGenerator(genLeafBool, genLeafInt, false)

  implicit lazy val arbExprBool : Arbitrary[Expr[Boolean]] = Arbitrary(genExpr.genExprBool)
  implicit lazy val arbExprInt : Arbitrary[Expr[Int]] = Arbitrary(genExpr.genExprInt)

  class EmptyComponent extends Component


  property("ConstantPropag") = forAll {(
      fBoolDef : Expr[Boolean], 
      fIntDef : Expr[Int]
    ) =>

      val comp = EmptyComponent()

      comp.svar ++= Iterable(
        sBool, 
        sInt, 
      )

      comp.fvar.out ++= Iterable(
        fBool, 
        fInt, 
      )

      given as ArrayBuffer[FlowAssertion[_]] = comp.assert.assertions
      fBool := fBoolDef
      fInt := fIntDef
      Prop.classify(fBoolDef != propagate(fBoolDef), "Propag Different"){
        compCheck(comp) match
          case Right(cm) => 
            val state = mutableInitialState(cm)
            (eval(fBoolDef, state) == eval(propagate(fBoolDef), state)) && 
            (eval(fIntDef, state) == eval(propagate(fIntDef), state))
          case Left(err) =>
            true
      }
  }
}