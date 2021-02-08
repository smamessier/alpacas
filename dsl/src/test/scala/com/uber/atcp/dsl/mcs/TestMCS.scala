package com.uber.atcp.dsl.mcs

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Assertions._

import scala.language.implicitConversions
import com.uber.atcp.dsl._
import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl.testtags._
import SeriesParallel.DirectConnections

import scala.annotation.tailrec
import scala.util.Random

/** Tests the dfs and bfs minimal sequence generation, 
  * by comparing their results with a naive generate-and-test
  * algorithm that generates all event sequences of bounded lenght upfront 
  * and runs them using the simulator.
  * */
class TestMCS extends AnyFlatSpec {
  /** Generates all sequences of requested size with repetitions.
    *
    * @tparam A type of elements of the sequences
    * @param size   size of the generated sequences
    * @param as     set of elements to generate sequences with
    * @param acc    accumulator for sequence currently being extended
    * @param resAcc final result accumulator for 
    * */
  def genSequences[A](as: Set[A], size: Int): Set[List[A]] = {
    require(size > 0)

    def loop(acc: List[A], resAcc: Set[List[A]]): Set[List[A]] = {
      // extend current prefix with each possible element
      val xs = as.map(a => a :: acc)
      if acc.size == size - 1
      then resAcc ++ xs
      else xs.foldLeft(resAcc) {
        case (resAcc, x) => loop(x, resAcc)
      }
    }

    loop(Nil, Set.empty)
  }

  // generate boolean series parallel systems with up to n components

  val n = 4
  val rng = scala.util.Random(seed = 0)
  val dists = Vector(
    InputDistribution.Dirac(1.0),
    InputDistribution.Exponential(10.0),
    InputDistribution.Exponential(100.0),
    InputDistribution.Exponential(1000.0),
    InputDistribution.Exponential(1e-2),
    InputDistribution.Exponential(1e-3),
    InputDistribution.Exponential(1e-4),
    InputDistribution.Exponential(1e-5)
  )
  
  given Ord[Boolean] {
    def lt(x: Boolean, y: Boolean): Boolean = (!x && y)
  }
  import SeriesParallel.{CompSpec, genAllSystems}
  val (bestMode, worstMode) = (true, false)
  val components = (1 to n).map(CompSpec(_,worstMode, dists(rng.nextInt(dists.size)))).toList
  val systems = genAllSystems(components, bestMode, worstMode)

  // systems with shortcut connections / feedback loops
  val dc = DirectConnections(
    List(
      (1,n),
      (2,n-1),
    ))
  val systemsFB = genAllSystems(components, bestMode, worstMode, dc)

  /** checks if two lists of minimal sequences contain the same elements. */
  def checkEqualSeq(l1: List[MinimalSequence], l2: List[MinimalSequence]): Boolean = {
    l1.forall(l2.contains(_)) && l2.forall(l1.contains(_))
  }

  /** checks if two lists of minimal cutsets contain the same elements. */
  def checkEqualSet(l1: List[MinimalCutSet], l2: List[MinimalCutSet]): Boolean = {
    l1.forall(l2.contains(_)) && l2.forall(l1.contains(_))
  }

  def checkMinimalSeq(s : List[MinimalSequence]) : Boolean = {
    s.forall{seq => s.count{other => subSequence(seq.events.map(_.id), other.events.map(_.id))} == 1}
  }

  def checkMinimalSet(s : List[MinimalCutSet]) : Boolean = {
    s.forall{seq => s.count{other => other.events.toSet.subsetOf(seq.events.toSet)} == 1}
  }

  "DFS and BFS minimal sequence algorithms" should "give the same results on feedforward systems" taggedAs(Slow, EquivalenceCheck) in {
    systems foreach { case system =>
      compCheck(system).fold(
        e => assert(false, s"error during computation: $e"),
        model =>
          for { 
            mcsDFS <- minimalCutSequencesDFS(m = model, observer = system.obs, n)
            mcsBFS <- minimalCutSequencesBFS(m = model, observer = system.obs, n)
          }{
            assert(checkEqualSeq(mcsDFS, mcsBFS), s"DFS result : \n${mcsDFS.mkString("\n")}\n is different from BFS result : \n${mcsBFS.mkString("\n")}\n")
            assert(checkMinimalSeq(mcsDFS), s"DFS result is not minimal : \n${mcsDFS.mkString("\n")}\n")
          }
      )
    }
  }

  "DFS and BFS minimal sequence algorithms with maxSize" should "give the same results on feedforward systems" taggedAs(Slow, EquivalenceCheck) in {
    systems foreach { case system =>
      compCheck(system).fold(
        e => assert(false, s"error during computation: $e"),
        model =>
          for { 
            mcsDFS <- minimalCutSequencesDFS(m = model, observer = system.obs, n/2)
            mcsBFS <- minimalCutSequencesBFS(m = model, observer = system.obs, n/2)
          }{
            assert(checkMinimalSeq(mcsDFS), s"DFS result is not minimal : \n$mcsDFS\n")
            assert(
              checkEqualSeq(mcsDFS, mcsBFS), 
              s"DFS result : \n${mcsDFS.mkString("\n")}\n is different from BFS result : \n${mcsBFS.mkString("\n")}\n"
            )
          }
      )
    }
  }

  "DFS and BFS minimal cutsets algorithms" should "give the same results on feedforward systems" taggedAs(Slow, EquivalenceCheck) in {
    systems foreach { case system =>
      compCheck(system).fold(
        e => assert(false, s"error during computation: $e"),
        model =>
          for { 
            mcsDFS <- minimalCutSetsDFS(m = model, observer = system.obs, n)
            mcsBFS <- minimalCutSetsBFS(m = model, observer = system.obs, n)
          }{
            assert(checkMinimalSet(mcsDFS), s"DFS result is not minimal : \n${mcsDFS.mkString("\n")}\n")
            assert(
              checkEqualSet(mcsDFS, mcsBFS), 
              s"DFS result : \n${mcsDFS.mkString("\n")}\n is different from BFS result : \n${mcsBFS.mkString("\n")}\n"
            )
          }
      )
    }
  }

  "DFS and BFS minimal cutsets algorithms with maxSize" should "give the same results on feedforward systems" taggedAs(Slow, EquivalenceCheck) in {
    systems foreach { case system =>
      compCheck(system).fold(
        e => assert(false, s"error during computation: $e"),
        model =>
          for { 
            mcsDFS <- minimalCutSetsDFS(m = model, observer = system.obs, n/2)
            mcsBFS <- minimalCutSetsBFS(m = model, observer = system.obs, n/2)
          }{
            assert(checkMinimalSet(mcsDFS), s"DFS result is not minimal : \n${mcsDFS.mkString("\n")}\n")
            assert(
              checkEqualSet(mcsDFS, mcsBFS), 
              s"DFS result : \n${mcsDFS.mkString("\n")}\n is different from BFS result : \n${mcsBFS.mkString("\n")}\n"
            )
          }
      )
    }
  }

  "DFS and BFS minimal sequence algorithms" should "give the same results on feedback systems" taggedAs(Slow, EquivalenceCheck) in {
    systemsFB foreach { case system =>
      compCheck(system).fold(
        e => assert(false, s"error during computation: $e"),
        model =>
          for { 
            mcsDFS <- minimalCutSequencesDFS(m = model, observer = system.obs, n+2)
            mcsBFS <- minimalCutSequencesBFS(m = model, observer = system.obs, n+2)
          }{
            assert(checkMinimalSeq(mcsDFS), s"DFS result is not minimal : \n${mcsDFS.mkString("\n")}\n")
            assert(checkEqualSeq(mcsDFS, mcsBFS), s"DFS result : \n${mcsDFS.mkString("\n")}\n is different from BFS result : \n${mcsBFS.mkString("\n")}\n")
          }
      )
    }
  }

  "DFS and BFS minimal sequence algorithms with maxSize" should "give the same results on feedback systems" taggedAs(Slow, EquivalenceCheck) in {
    systemsFB foreach { case system =>
      compCheck(system).fold(
        e => assert(false, s"error during computation: $e"),
        model =>
          for { 
            mcsDFS <- minimalCutSequencesDFS(m = model, observer = system.obs, n/2)
            mcsBFS <- minimalCutSequencesBFS(m = model, observer = system.obs, n/2)
          }{
            assert(checkMinimalSeq(mcsDFS), s"DFS result is not minimal : \n${mcsDFS.mkString("\n")}\n")
            assert(checkEqualSeq(mcsDFS, mcsBFS), s"DFS result : \n${mcsDFS.mkString("\n")}\n is different from BFS result : \n${mcsBFS.mkString("\n")}\n")
          }
      )
    }
  }

  "DFS and BFS minimal cutsets algorithms" should "give the same results on feedback systems" taggedAs(Slow, EquivalenceCheck) in {
    systemsFB foreach { case system =>
      compCheck(system).fold(
        e => assert(false, s"error during computation: $e"),
        model =>
          for { 
            mcsDFS <- minimalCutSetsDFS(m = model, observer = system.obs, n+2)
            mcsBFS <- minimalCutSetsBFS(m = model, observer = system.obs, n+2)
          }{
            assert(checkMinimalSet(mcsDFS), s"DFS result is not minimal : \n${mcsDFS.mkString("\n")}\n")
            assert(
              checkEqualSet(mcsDFS, mcsBFS), 
              s"DFS result : \n${mcsDFS.mkString("\n")}\n is different from BFS result : \n${mcsBFS.mkString("\n")}\n"
            )
          }
      )
    }
  }

  "DFS and BFS minimal cutsets algorithms with maxSize" should "give the same results on feedback systems" taggedAs(Slow, EquivalenceCheck) in {
    systemsFB foreach { case system =>
      compCheck(system).fold(
        e => assert(false, s"error during computation: $e"),
        model =>
          for { 
            mcsDFS <- minimalCutSetsDFS(m = model, observer = system.obs, n/2)
            mcsBFS <- minimalCutSetsBFS(m = model, observer = system.obs, n/2)
          }{
            assert(checkMinimalSet(mcsDFS), s"DFS result is not minimal : \n${mcsDFS.mkString("\n")}\n")
            assert(
              checkEqualSet(mcsDFS, mcsBFS), 
              s"DFS result : \n${mcsDFS.mkString("\n")}\n is different from BFS result : \n${mcsBFS.mkString("\n")}\n"
            )
          }
      )
    }
  }
}
