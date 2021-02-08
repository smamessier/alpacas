package com.uber.atcp.dsl.exprEval

import com.uber.atcp.dsl.state._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl._
import com.uber.atcp.dsl.frontend.{_, given _}


type Id[T] = T

class UndefinedFVRead(val name : String) extends Error

/** Reduction for expression evaluation
  */
def evalred(state : State) = new Reduction[Id] {
  def rFvar[T](x : Expr.Fvar[T]) : T = state.readFV(x) match 
    case Some(t) => t 
    case None => throw UndefinedFVRead(x.name)
  def rSvar[T](x : Expr.Svar[T]) : T = state.readSV(x)
  def rConst[T](x : Expr.Const[T]) : T = x.value
  def rEq[T](x : T, y : T) : Boolean = x == y
  def rIte[T](c : Boolean, t : T, e : T) : T = if c then t else e
  def rLt[T : Ord](l : T, r : T) : Boolean = summon[Ord[T]].lt(l, r)
  def rNumBin[T : Numeric](b : NumBinop, l : T, r : T) : T = b match
    case NumBinop.Add => implicitly[Numeric[T]].add(l, r)
    case NumBinop.Sub => implicitly[Numeric[T]].sub(l, r)
    case NumBinop.Mult => implicitly[Numeric[T]].mult(l, r)
    case NumBinop.Div => implicitly[Numeric[T]].div(l, r)
  def rLogBin[T : Logic](b : LogBinop, l : T, r : T) : T = b match
    case LogBinop.Or => implicitly[Logic[T]].or(l, r)
    case LogBinop.And => implicitly[Logic[T]].and(l, r)
  def rUn[T : Logic](u : Unop, e : T) : T = u match
    case Unop.Neg => implicitly[Logic[T]].not(e)
} 

/** Evaluation for expressions
  * @param e expression to evaluate
  * @param state variable values environment
  * @return evaluated expression
  */
def eval[T](e : Expr[T], state : State) : T = cata(e, evalred(state))
