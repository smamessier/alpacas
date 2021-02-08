package com.uber.atcp.dsl.mcs

import scala.language.implicitConversions
import scala.util.control.TailCalls._
import scala.collection.mutable.Queue

import com.uber.atcp.dsl._
import com.uber.atcp.dsl.transition.EventIds._
import com.uber.atcp.dsl.ast.Expr
import com.uber.atcp.dsl.exprEval._ 
import com.uber.atcp.dsl.state.State
import com.uber.atcp.dsl.step._
import com.uber.atcp.dsl.stateImpl.{immutableInitialState}
import com.uber.atcp.dsl.errors._


/** Recursive depth-first search for cut sequences enumeration
  * @param state origin state
  * @param observer failure condition variable
  * @param sequence sequence of events leading to current state 
  * @param sequences list of found cut sequences
  * @return list of cut sequences
  */
def dfs(
  m : CausalModel, 
  state : StableState, 
  observer : Expr[Boolean],
  sequence : List[EventId], 
  sequences : EitherResult[List[List[EventId]]],
  fireableUrgent : EventSet,
  maxSize : Int,
) : EitherResult[List[List[EventId]]] = {
  if eval(observer, state) 
    if sequence.size >= maxSize
      sequences 
    else
      val ftrans = fireable(m, state)
      ftrans.foldLeft(sequences){case (seqs, ft) =>
        fire(m, state, ft.id, fireableUrgent).flatMap{newState =>
          dfs(m, newState, observer, ft.id::sequence, seqs, fireableUrgent, maxSize)
        }
      }
  else
    sequences.flatMap{sequences =>
      Right(sequence::sequences)
    }
}

def minimize(seqs : List[List[EventId]]) : List[List[EventId]] = {
  def aux(l : List[List[EventId]]) : TailRec[List[List[EventId]]] = {
    if l.isEmpty 
      then done(List.empty)
      else for { 
          res <-  tailcall(aux(l.tail filter {seq => !subSequence(l.head, seq)}))
        } yield l.head :: res
  }

  val sorted = seqs.sortBy(e => e.size)
  aux(sorted).result
}

/** @param seqs list of cut sequences
  * @return list of minimal sequences */
def minimizeSeq(seqs : List[List[EventId]], m : CausalModel): List[MinimalSequence] = {
  minimize(seqs).map(evids => MinimalSequence(evids, m))
}

extension (mcs : MinimalCutSet)
  def inc(other : MinimalCutSet) =
    mcs.events.forall{other.events.contains(_)}

def subSequence(l1 : List[EventId], l2 : List[EventId]) : Boolean = {
  if l1.size > l2.size
    false
  else
    l1 match
      case t1::q1 =>
        l2 match
          case t2::q2 =>
            if t1 == t2
              subSequence(q1, q2)
            else
              subSequence(l1, q2)
          case Nil => 
            false
      case Nil => 
        true
}

/** @param seqs list of cut sequences
  * @return list of minimal cut sets */
def minimizeSets(seqs : List[List[EventId]], m : CausalModel) : List[MinimalCutSet] = {
  val sets = minimize(seqs).map{evids => MinimalCutSet(evids.toList, m)}
  def minSets(sets : List[MinimalCutSet], res : List[MinimalCutSet]) : List[MinimalCutSet] = {
    sets match
      case set::q => 
        if res exists {_.inc(set)}
          minSets(q, res)
        else
          minSets(q, set::res)
      case _ => 
        res
  }
  minSets(sets, List()).reverse
}

/** Breadth-first search for cut sequences enumeration
  * @param model model to analyse
  * @param observer failure condition variable
  * @return list of minimal sequences
  */
def bfs(
  model : CausalModel, 
  observer : Expr[Boolean],
  maxSize : Int,
  fireableUrgent : EventSet
) : EitherResult[List[List[EventId]]] = {
  val queue = Queue((immutableInitialState(model), List[EventId]()))
  var res : List[List[EventId]] = Nil
  while (!queue.isEmpty) {
    val (state, seq) = queue.dequeue()
    if !eval(observer, state) && !res.exists(minimalSeq => subSequence(minimalSeq, seq))
      res = seq::res
    else 
      if seq.size < maxSize
        val ftrans = fireable(model, state)
        ftrans.foreach{t => 
          val newSeq = t.id::seq
          if !res.exists(minimalSeq => subSequence(minimalSeq, newSeq))
            fire(model, state, t.id, fireableUrgent) match
              case Right(newState) =>
                queue.enqueue((newState, newSeq))
              case Left(e) =>
                return Left(e)
        }
  }
  Right(res)
}


trait MCS {
    
  /** Minimal cut sequences enumeration using naive depth-first search
    * @param m top-level component
    * @param observer failure condition variable
    * @param maxSize max size of sequences to consider 
    * @return list of minimal sequences
    */
  def minimalCutSequencesDFS(
    m : CausalModel, 
    observer : Expr[Boolean], 
    maxSize : Int
  ) : EitherResult[List[MinimalSequence]] = {
    val state = immutableInitialState(m)
    val fireableUrgent = EventSet(m)
    var res = dfs(m, state, observer, List(), Right(List()), fireableUrgent, maxSize)
    res.flatMap{res =>
      res.reverse.map(_.reverse)
      Right(minimizeSeq(res, m))
    }
  }

  /** Minimal cut sets enumeration using depth-first search
    * @param m top-level component
    * @param observer failure condition variable
    * @param maxSize max size of sets to consider
    * @return list of minimal cut sets
    */
  def minimalCutSetsDFS(
    m : CausalModel, 
    observer : Expr[Boolean], 
    maxSize : Int
  ) : EitherResult[List[MinimalCutSet]] = {
    val state = immutableInitialState(m)
    val fireableUrgent = EventSet(m)
    var res = dfs(m, state, observer, List(), Right(List()), fireableUrgent, maxSize)
    res.flatMap{res =>
      res.reverse.map(_.reverse)
      Right(minimizeSets(res, m))
    }
  }

  /** Minimal cut sequences enumeration using breadth-first search
    * @param m top-level component
    * @param observer failure condition variable
    * @param maxSize max size of sequences to consider 
    * @return list of minimal sequences
    */
  def minimalCutSequencesBFS(
    m : CausalModel, 
    observer : Expr[Boolean],
    maxSize : Int
  ) : EitherResult[List[MinimalSequence]] = {
    val fireableUrg = EventSet(m)
    bfs(m, observer, maxSize, fireableUrg).flatMap{res =>
      Right(res.map(_.reverse).reverse.map(MinimalSequence(_, m)))
    }
  }

  /** Minimal cut sequences enumeration using naive depth-first search
    * @param m top-level component
    * @param observer failure condition variable
    * @param maxSize max size of sequences to consider 
    * @return set of minimal cut sets
    */
  def minimalCutSetsBFS(
    m : CausalModel, 
    observer : Expr[Boolean], 
    maxSize : Int
  ) : EitherResult[List[MinimalCutSet]] = {
    val fireableUrg = EventSet(m)
    bfs(m, observer, maxSize, fireableUrg).flatMap{res =>
      Right(minimizeSets(res.map(_.reverse).reverse, m))
    }
  }

}
