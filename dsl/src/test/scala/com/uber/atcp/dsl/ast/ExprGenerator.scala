package com.uber.atcp.dsl.ast

/** Validation strategy for ast package:
  * We represent expressions in a Generalized algebraic data type (GADT). 
  * It means that only well-typed expressions can be represented in our models. */

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import com.uber.atcp.dsl.Modelling.{_, given _}

/** Scalacheck generators for Int and Boolean expressions to be used in property-based tests
  * @param genLeafBool generator for Boolean Fvar and Svar (variable instances from a model are needed)
  * @param genLeafInt generator for Int Fvar and Svar
  */
class ExprGenerator(genLeafBool : Gen[Expr[Boolean]], genLeafInt : Gen[Expr[Int]], div : Boolean) {

  def genExprBool : Gen[Expr[Boolean]] = {
    lazy val genIteBool : Gen[Expr[Boolean]] = for {
      cond <- genExprBool
      trueBranch <- genExprBool
      falseBranch <- genExprBool
    } yield Expr.Ite(cond, trueBranch, falseBranch)

    lazy val genLogBinop : Gen[LogBinop] = Gen.oneOf(LogBinop.And, LogBinop.Or)

    def genUn : Gen[Expr[Boolean]] = for (e <- genExprBool) yield Expr.Un(Unop.Neg, e)

    def genLogBin : Gen[Expr[Boolean]] = for {
      op <- genLogBinop
      l <- genExprBool
      r <- genExprBool
    } yield Expr.LogBin(op, l, r)

    def genEqInt : Gen[Expr[Boolean]] = for {
      l <- genExprInt
      r <- genExprInt
    } yield Expr.Eq(l, r)

    def genLtInt : Gen[Expr[Boolean]] = for {
      l <- genExprInt
      r <- genExprInt
    } yield Expr.Lt(l, r)

    def genBranchBool : Gen[Expr[Boolean]] = 
      Gen.lzy(Gen.frequency(
        (1, genIteBool), 
        (2, genLogBin), 
        (1, genUn), 
        (1, genEqInt), 
        (1, genLtInt))
      )

    Gen.lzy(Gen.frequency((2, genBranchBool), (3, genLeafBool)))
  }


  def genExprInt : Gen[Expr[Int]] = {
    lazy val genIte : Gen[Expr[Int]] = for {
      cond <- genExprBool
      trueBranch <- genExprInt
      falseBranch <- genExprInt
    } yield Expr.Ite(cond, trueBranch, falseBranch)

    lazy val genNumBinop : Gen[NumBinop] = 
      if div 
        Gen.oneOf(NumBinop.Add, NumBinop.Sub, NumBinop.Mult, NumBinop.Div) // pb : div by 0
      else
        Gen.oneOf(NumBinop.Add, NumBinop.Sub, NumBinop.Mult)

    def genNumBin : Gen[Expr[Int]] = for {
      op <- genNumBinop
      l <- genExprInt
      r <- genExprInt
    } yield Expr.NumBin(op, l, r)

    def genBranchInt : Gen[Expr[Int]] = Gen.lzy(Gen.frequency((1, genIte), (4, genNumBin)))

    Gen.lzy(Gen.frequency((2, genBranchInt), (3, genLeafInt)))
  }
}
