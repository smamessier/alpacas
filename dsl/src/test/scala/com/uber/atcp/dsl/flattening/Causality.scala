package com.uber.atcp.dsl.flattening

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import scala.collection.mutable.ArrayBuffer

import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.ast.StateIds._
import com.uber.atcp.dsl.ast.FlowIds._
import com.uber.atcp.dsl.assertion._
import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._

import CausalityTests._

/** We build models with variable right-hand sides for the assertions (filled with generated expressions), 
  * and test that the outcome of causality checks satisfy the requirements given by module CausalModel. */
object CausalitySpec extends Properties("Causality") {

  val sBool1 = Expr.Svar(StateId(), true, "sbool1", 12).asInstanceOf[Expr.Svar[Boolean]]
  val sBool2 = Expr.Svar(StateId(), false, "sbool2", 13).asInstanceOf[Expr.Svar[Boolean]]

  val fBool1 = Expr.Fvar[Boolean](FlowId(), "fbool1", 15).asInstanceOf[Expr.Fvar[Boolean]]
  val fBool2 = Expr.Fvar[Boolean](FlowId(), "fbool2", 16).asInstanceOf[Expr.Fvar[Boolean]]

  val genSVBool = oneOf(sBool1, sBool2)
  val genFVBool = oneOf(fBool1, fBool2)

  val genLeafBool : Gen[Expr[Boolean]] = oneOf(genSVBool, genFVBool, arbitrary[Boolean].flatMap(Expr.Const(_)))


  val sInt1 = Expr.Svar(StateId(), 1, "sInt1", 50).asInstanceOf[Expr.Svar[Int]]
  val sInt2 = Expr.Svar(StateId(), 0, "sInt2", 51).asInstanceOf[Expr.Svar[Int]]

  val fInt1 = Expr.Fvar[Int](FlowId(), "fInt1", 53).asInstanceOf[Expr.Fvar[Int]]
  val fInt2 = Expr.Fvar[Int](FlowId(), "fInt2", 54).asInstanceOf[Expr.Fvar[Int]]

  val genSVInt = oneOf(sInt1, sInt2)
  val genFVInt = oneOf(fInt1, fInt2)
  val genLeafInt : Gen[Expr[Int]] = oneOf(genSVInt, genFVInt, arbitrary[Int].flatMap(Expr.Const(_)))

  val genExpr = ExprGenerator(genLeafBool, genLeafInt, true)

  implicit lazy val arbExprBool : Arbitrary[Expr[Boolean]] = Arbitrary(genExpr.genExprBool)
  implicit lazy val arbExprInt : Arbitrary[Expr[Int]] = Arbitrary(genExpr.genExprInt)

  class EmptyComponent extends Component

  property("CausalityCorrect") = forAll {(
      fBool1Def : Expr[Boolean], 
      fBool2Def : Expr[Boolean], 
      fInt1Def : Expr[Int], 
      fInt2Def : Expr[Int]
    ) =>

      val comp = EmptyComponent()

      comp.svar ++= Iterable(
        sBool1, 
        sBool2, 
        sInt1, 
        sInt2
      )

      comp.fvar.out ++= Iterable(
        fBool1, 
        fBool2, 
        fInt1, 
        fInt2
      )

      given as ArrayBuffer[FlowAssertion[_]] = comp.assert.assertions
      fBool1 := fBool1Def
      fBool2 := fBool2Def
      fInt1 := fInt1Def
      fInt2 := fInt2Def

      val flowDepsMap = Map[String, Set[String]](
        (fBool1.name, variables(fBool1Def)),
        (fBool2.name, variables(fBool2Def)),
        (fInt1.name, variables(fInt1Def)),
        (fInt2.name, variables(fInt2Def))
      )

      compCheck(comp) match
        case Right(cm) => 
          Prop.classify(true, "Causal"){
            checkTopo(cm, flowDepsMap)
          } 
        case Left(err) =>
          Prop.classify(true, "Cyclic"){
            checkErrors(err, flowDepsMap)
          }

  }
}