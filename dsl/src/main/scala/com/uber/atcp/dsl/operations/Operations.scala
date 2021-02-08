package com.uber.atcp.dsl.operations

/** Order definition for DSL types */
trait Ord[T] {
  def lt(x : T, y : T) : Boolean
}

trait Logic[T] {
  def and(x : T, y : T) : T
  def or(x : T, y : T) : T
  def not(x : T) : T
}

trait Numeric[T] {
  def add(x : T, y : T) : T
  def sub(x : T, y : T) : T
  def mult(X : T, y : T) : T
  def div(x : T, y : T) : T
}