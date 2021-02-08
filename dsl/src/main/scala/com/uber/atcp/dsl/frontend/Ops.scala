package com.uber.atcp.dsl.frontend

import scala.deriving._
import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer

import com.uber.atcp.dsl._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.assertion._
import com.uber.atcp.dsl.transition._

trait ExpressionFactory {
  /** Implicit conversion for constants lifting in AST */
  given [T] as Conversion[T, Expr[T]] {
    def apply(t:T): Expr[T] = Expr.Const(t)
  }

  /** Implicit conversion from Cut Sequences to Cut Sets */
  given as Conversion[MinimalSequence, MinimalCutSet] {
    def apply(s : MinimalSequence): MinimalCutSet = 
      MinimalCutSet(s)

  }

  def If(c : Expr[Boolean]) : Ift =
    Ift(c)

  extension [T](i : Ift)
    def Then (t : Expr[T]) : IfThent[T] =
      IfThent(i.c, t)

  extension [T](it : IfThent[T])
    def Else (e : Expr[T]) : Expr[T] =
      Expr.Ite(it.c, it.t, e)
    def ElseIf (c : Expr[Boolean]) : ElseIft[T] =
      ElseIft(List(c, it.c), List(it.t))

  extension [T](eit : ElseIfThent[T])
    def ElseIf (c : Expr[Boolean]) : ElseIft[T] =
      ElseIft(c::eit.c, eit.t)
    def Else (e : Expr[T]) : Expr[T] =
      eit match
        case ElseIfThent(c1::c, t1::t) => ElseIfThent(c, t) Else Expr.Ite(c1, t1, e)
        case _ => e

  extension [T](ei : ElseIft[T])
    def Then (t : Expr[T]) : ElseIfThent[T] = 
      ElseIfThent(ei.c, t::ei.t)


  /** Type class for types lifted in Alpacas */
  trait Lifted[T] {
    extension (x : Expr[T])
      def === (y : Expr[T]) : Expr[Boolean]
    extension (x : Expr.Svar[T])
      def := (y : Expr[T]) (using a : StateAssertionBuilder, line : sourcecode.Line) : Unit
    extension (x : Expr.Fvar[T])
      def := (y : Expr[T]) (using a : FlowAssertions, line : sourcecode.Line) : Unit
  }

  /** Type class derivation for user-defined types */
  object Lifted {
    def derived[T] : Lifted[T] = new Lifted[T] {
      extension (x : Expr[T])
        def === (y : Expr[T]): Expr[Boolean] = 
          Expr.Eq(x, y)
      extension (x : Expr.Svar[T])
        def := (y : Expr[T]) (using a : StateAssertionBuilder, line : sourcecode.Line) : Unit = 
          a += StateAssertion(x, y, line.value)
      extension (x : Expr.Fvar[T])
        def := (y : Expr[T]) (using a : FlowAssertions, line : sourcecode.Line) : Unit = 
          a += FlowAssertion(x, y, line.value)
    }
  }

  extension [T : Lifted](x : Vector[Expr.Fvar[T]])
    def := (y : Vector[Expr[T]])(using assert : FlowAssertions, line : sourcecode.Line) = 
      if (x.size != y.size)
        throw VectorAssignementSizeError(line.value)
      x zip y foreach ((a,b) => a := b)

  given Lifted[Boolean] = Lifted.derived
  given Lifted[Int] = Lifted.derived
  given Lifted[Double] = Lifted.derived

  /** Lifted order for Expressions */
  trait DSLord[T : Lifted] {
    extension (x : Expr[T])
      def < (y : Expr[T]) : Expr[Boolean] 
      def > (y : Expr[T]) : Expr[Boolean] = !(x < y) && !(x === y)
      def <= (y : Expr[T]) : Expr[Boolean] = x < y || x === y
      def >= (y : Expr[T]) : Expr[Boolean] = !(x < y)
      def min (y : Expr[T]) : Expr[T] = If (x < y) Then x Else y
      def max (y : Expr[T]) : Expr[T] = If (x < y) Then y Else x
  }

  trait LeftDSLord[T : Lifted] {
    extension (x : T) 
      def < (y : Expr[T]) : Expr[Boolean] 
      def > (y : Expr[T]) : Expr[Boolean] = !(x < y) && !(y === x)
      def <= (y : Expr[T]) : Expr[Boolean] = x < y || y === x
      def >= (y : Expr[T]) : Expr[Boolean] = !(x < y)
      def min (y : Expr[T]) : Expr[T] = If (x < y) Then x Else y
      def max (y : Expr[T]) : Expr[T] = If (x < y) Then y Else x
      def < (y : T) : Boolean 
      def > (y : T) : Boolean = !(x < y) && !(y == x)
      def <= (y : T) : Boolean = x < y || y == x
      def >= (y : T) : Boolean = !(x < y)
      def min (y : T) : T = if (x < y) then x else y
      def max (y : T) : T = if (x < y) then y else x
  }

  given Ord[Int] {
    def lt(x : Int, y : Int) : Boolean = x < y
  }

  given Ord[Double] {
    def lt(x : Double, y : Double) : Boolean = x < y
  }

  given [T : Lifted : Ord] as DSLord[T] {
    extension (x : Expr[T])
      def < (y : Expr[T]) : Expr[Boolean] = Expr.Lt(x, y)
  }

  given [T : Lifted : Ord] as LeftDSLord[T] {
    extension (x : T)
      def < (y : Expr[T]) : Expr[Boolean] = Expr.Lt(Expr.Const(x), y)
      def < (y : T) : Boolean = summon[Ord[T]].lt(x, y)
  }

  /** Lifted boolean operations for expressions */
  trait DSLlogic[T] {
    extension (x : Expr[T])
      def && (y : Expr[T]) : Expr[T]
      def || (y : Expr[T]) : Expr[T]
      def unary_! : Expr[T]
  }

  trait LeftDSLlogic[T] {
    extension (x : T)
      def && (y : Expr[T]) : Expr[T]
      def || (y : Expr[T]) : Expr[T]
  }


  given [T : Logic] as DSLlogic[T] {
    extension (x : Expr[T])
      def && (y : Expr[T]) : Expr[T] = Expr.LogBin(LogBinop.And, x, y)
      def || (y : Expr[T]) : Expr[T] = Expr.LogBin(LogBinop.Or, x, y)
      def unary_! : Expr[T] = Expr.Un(Unop.Neg, x)
  }

  given [T : Logic] as LeftDSLlogic[T] {
    extension (x : T)
      def && (y : Expr[T]) : Expr[T] = Expr.LogBin(LogBinop.And, Expr.Const(x), y)
      def || (y : Expr[T]) : Expr[T] = Expr.LogBin(LogBinop.Or, Expr.Const(x), y)
  }

  given Logic[Boolean] {
    def and(x : Boolean, y : Boolean) : Boolean = x && y
    def or(x : Boolean, y : Boolean) : Boolean = x || y
    def not(x : Boolean) : Boolean = !x
  }

  /** Lifted numeric operations for expressions */
  trait DSLNumeric[T] {
    extension (x : Expr[T])
      def + (y : Expr[T]) : Expr[T]
      def - (y : Expr[T]) : Expr[T]
      def * (y : Expr[T]) : Expr[T]
      def / (y : Expr[T]) : Expr[T]
  }

  trait LeftDSLNumeric[T] {
    extension (x : T)
      def + (y : Expr[T]) : Expr[T] 
      def - (y : Expr[T]) : Expr[T] 
      def * (y : Expr[T]) : Expr[T] 
      def / (y : Expr[T]) : Expr[T] 
  }

  given [T : Numeric] as DSLNumeric[T] {
    extension (x : Expr[T])
      def + (y : Expr[T]) : Expr[T] = Expr.NumBin(NumBinop.Add, x, y)
      def - (y : Expr[T]) : Expr[T] = Expr.NumBin(NumBinop.Sub, x, y)
      def * (y : Expr[T]) : Expr[T] = Expr.NumBin(NumBinop.Mult, x, y)
      def / (y : Expr[T]) : Expr[T] = Expr.NumBin(NumBinop.Div, x, y)
  }

  given [T : Numeric] as LeftDSLNumeric[T] {
    extension (x : T)
      def + (y : Expr[T]) : Expr[T] = Expr.NumBin(NumBinop.Add, Expr.Const(x), y)
      def - (y : Expr[T]) : Expr[T] = Expr.NumBin(NumBinop.Sub, Expr.Const(x), y)
      def * (y : Expr[T]) : Expr[T] = Expr.NumBin(NumBinop.Mult, Expr.Const(x), y)
      def / (y : Expr[T]) : Expr[T] = Expr.NumBin(NumBinop.Div, Expr.Const(x), y)
  }


  given Numeric[Int]{
    def add(x : Int, y : Int) = x + y
    def sub(x : Int, y : Int) = x - y
    def mult(x : Int, y : Int) =  x * y
    def div(x : Int, y : Int) = x / y
  }

  given Numeric[Double]{
    def add(x : Double, y : Double) = x + y
    def sub(x : Double, y : Double) = x - y
    def mult(x : Double, y : Double) =  x * y
    def div(x : Double, y : Double) = x / y
  }

  def Switch[T, U] (e : Expr[T]) (init : ArrayBuffer[Case[T,U]] ?=> Deflt[U]) : Expr[U] = {
    given cases as ArrayBuffer[Case[T,U]]
    val d = init
    var res = d.v
    for (i <- cases.reverseIterator){
      res = Expr.Ite(Expr.Eq(e ,i.t), i.u, res)
    }
    res
  }

  extension [T,U] (t : Expr[T])
    def ->(u : Expr[U])(using cs : ArrayBuffer[Case[T,U]]) = cs += Case(t, u)

  def Default[U](v : Expr[U]) : Deflt[U] = Deflt(v)
}
