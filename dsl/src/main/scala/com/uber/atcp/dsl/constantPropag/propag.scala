package com.uber.atcp.dsl.constantPropag

import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl._



/** Reduction for propagation of boolean constants in expressions
  */
def propagred = new Reduction[Expr] {
  def rFvar[T](x : Expr.Fvar[T]) : Expr[T] = x
  def rSvar[T](x : Expr.Svar[T]) : Expr[T] = x
  def rConst[T](x : Expr.Const[T]) : Expr[T] = x
  def rEq[T](x : Expr[T], y : Expr[T]) : Expr[Boolean] = (x, y) match
    case (Expr.Const(a), Expr.Const(b)) => Expr.Const(a == b)
    case _ => Expr.Eq(x, y)
  def rIte[T](c : Expr[Boolean], t : Expr[T], e : Expr[T]) : Expr[T] = c match
    case Expr.Const(b) => if b then t else e
    case _ => Expr.Ite(c, t, e)
  def rLt[T : Ord](l : Expr[T], r : Expr[T]) : Expr[Boolean] = (l, r) match
    case (Expr.Const(a), Expr.Const(b)) => Expr.Const(summon[Ord[T]].lt(a, b))
    case _ => Expr.Lt(l, r)
  def rNumBin[T : Numeric](b : NumBinop, l : Expr[T], r : Expr[T]) : Expr[T] = (l, r) match
    case (Expr.Const(vl), Expr.Const(vr)) => b match
      case NumBinop.Add => Expr.Const(implicitly[Numeric[T]].add(vl, vr))
      case NumBinop.Sub => Expr.Const(implicitly[Numeric[T]].sub(vl, vr))
      case NumBinop.Mult => Expr.Const(implicitly[Numeric[T]].mult(vl, vr))
      case NumBinop.Div => Expr.Const(implicitly[Numeric[T]].div(vl, vr))
    case _ => Expr.NumBin(b, l, r)
  def rLogBin[T : Logic](b : LogBinop, l : Expr[T], r : Expr[T]) : Expr[T] = (l, r) match
    case (Expr.Const(vl), Expr.Const(vr)) => b match
      case LogBinop.Or => Expr.Const(implicitly[Logic[T]].or(vl, vr))
      case LogBinop.And => Expr.Const(implicitly[Logic[T]].and(vl, vr))
    case _ => Expr.LogBin(b, l, r)
  def rUn[T : Logic](u : Unop, e : Expr[T]) : Expr[T] = e match 
    case Expr.Const(ve) => u match
      case Unop.Neg => Expr.Const(implicitly[Logic[T]].not(ve))
    case _ => Expr.Un(u, e)
}

/** Boolean constant propagation
  * @param e expression to simplify
  * @return expression with boolean constants simplification
  */
def propagate[T](e : Expr[T]) : Expr[T] = cata(e, propagred)