package com.uber.atcp.dsl.ast

import scala.collection.mutable._
import scala.language.implicitConversions

import java.util.UUID

import com.uber.atcp.dsl._


object FlowIds {
  opaque type FlowId = UUID

  object FlowId {
    def apply() : FlowId = UUID.randomUUID()
  }
}

import FlowIds._

object StateIds {
  opaque type StateId = UUID

  object StateId {
    def apply() : StateId = UUID.randomUUID()
  }
}

import StateIds._

/** GADT for expressions internal representation */
enum Expr[T] {
  case Svar(uid : StateId, init : T, name : String, line : Int) extends Expr[T]
  case Fvar(uid : FlowId, name : String, line : Int) extends Expr[T]
  case Const(value : T) extends Expr[T]
  case Eq(l : Expr[T], r : Expr[T]) extends Expr[Boolean]
  case Ite(c : Expr[Boolean], t : Expr[T], e : Expr[T]) extends Expr[T]
  case Lt(l : Expr[T], r : Expr[T])(using val ev : Ord[T]) extends Expr[Boolean]
  case NumBin(b : NumBinop, l : Expr[T], r : Expr[T])(using val ev : Numeric[T]) extends Expr[T]
  case LogBin(b : LogBinop, l : Expr[T], r : Expr[T])(using val ev : Logic[T]) extends Expr[T]
  case Un(u : Unop, e : Expr[T])(using val ev : Logic[T]) extends Expr[T]
  def toString(n : Int) : String = {
    this match
      case Eq(l, r) => s"${"  "*n}Eq(\n${l.toString(n+1)},\n${r.toString(n+1)})"
      case Ite(c, t, e) => s"${"  "*n}Ite(\n${c.toString(n+1)},\n${t.toString(n+1)},\n${e.toString(n+1)})"
      case Lt(l, r) => s"${"  "*n}Lt(\n${l.toString(n+1)},\n${r.toString(n+1)})"
      case NumBin(op, l, r) => s"${"  "*n}$op(\n${l.toString(n+1)},\n${r.toString(n+1)})"
      case LogBin(op, l, r) => s"${"  "*n}$op(\n${l.toString(n+1)},\n${r.toString(n+1)})"
      case Un(u, e) => s"${"  "*n}$u(\n${e.toString(n+1)})"
      case _ => s"${"  "*n}$this"
  }
}

enum NumBinop {
  case Add
  case Sub
  case Mult 
  case Div
}

enum LogBinop {
  case Or 
  case And
}

enum Unop {
  case Neg
}

case class Ift(c : Expr[Boolean])
case class IfThent[T](c : Expr[Boolean], t : Expr[T])
case class ElseIft[T](c : List[Expr[Boolean]], t : List[Expr[T]])
case class ElseIfThent[T](c : List[Expr[Boolean]], t : List[Expr[T]])

case class Case[T,U](t : Expr[T], u : Expr[U])
case class Deflt[U](v : Expr[U])
