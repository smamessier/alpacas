package com.uber.atcp.dsl.ast


import com.uber.atcp.dsl._

/** Type class for Reduction functions of catamorphisms
  */
trait Reduction[F[_]] {
  def rFvar[T](x : Expr.Fvar[T]) : F[T] 
  def rSvar[T](x : Expr.Svar[T]) : F[T] 
  def rConst[T](x : Expr.Const[T]) : F[T] 
  def rEq[T](x : F[T], y : F[T]) : F[Boolean] 
  def rIte[T](c : F[Boolean], t : F[T], e : F[T]) : F[T]
  def rLt[T : Ord](l : F[T], r : F[T]) : F[Boolean]
  def rNumBin[T : Numeric](b : NumBinop, l : F[T], r : F[T]) : F[T]
  def rLogBin[T : Logic](b : LogBinop, l : F[T], r : F[T]) : F[T]
  def rUn[T : Logic](u : Unop, e : F[T]) : F[T]
}

/** Catamorphism application of Reduction on expression
 */
def cata[T, F[_]](x : Expr[T], red : Reduction[F]) : F[T] = {
  x match
    case x : Expr.Svar[T] => red.rSvar(x)
    case x : Expr.Fvar[T] => red.rFvar(x)
    case x : Expr.Const[T] => red.rConst(x)
    case Expr.Eq(l, r) => red.rEq(cata(l, red), cata(r, red))
    case Expr.Ite(c, t, e) => red.rIte(cata(c, red), cata(t, red), cata(e, red))
    case e @ Expr.Lt(l, r) => red.rLt(cata(l, red), cata(r, red))(e.ev)
    case e @ Expr.NumBin(b, l, r) => red.rNumBin(b, cata(l, red), cata(r, red))(e.ev)
    case e @ Expr.LogBin(b, l, r) => red.rLogBin(b, cata(l, red), cata(r, red))(e.ev)
    case e @ Expr.Un(u, se) => red.rUn(u,cata(se, red))(e.ev)
}