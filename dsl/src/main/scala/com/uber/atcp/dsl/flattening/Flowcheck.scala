package com.uber.atcp.dsl.flattening

import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.Iterable
import scala.language.implicitConversions
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import cats.{Applicative, Traverse}
import cats.data.Validated._
import cats.data._
import cats.implicits._

import com.uber.atcp.dsl._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.frontend._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.ast.FlowIds._
import com.uber.atcp.dsl.ast.StateIds._
import com.uber.atcp.dsl.transition.EventIds._
import com.uber.atcp.dsl.assertion.FlowAssertion
import com.uber.atcp.dsl.constantPropag._
import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.errors._


/** Propagation of boolean constants on guard and right members of transition */
def simplify(t : Transition) : Transition = {
  Transition(t.ev, propagate(t.guard), t.a.map{case StateAssertion(l, r, line) => StateAssertion(l, propagate(r), line)})
}
 
/** Verification of existence and unicity of variable assignations
  * Out flows and subcomponents' in flows need assignement
  * @param c top level component
  * @param idConverter for array indexing and dimensioning
  * @return (Map flow var -> corresponding assertion, variable names structure) or error 
  */
def varCheck(c : Component, idConverter : IdConverter) : EitherResult[(Map[Int,FlowAssertion[_]], VarNames)] = {
  val varNames = VarNames(
    Array.fill(idConverter.stateNumber)(""),
    Array.fill(idConverter.flowNumber)(""),
    Array.fill(idConverter.evNumber)(""),
    idConverter
  )
  nameVars(c, varNames, "")
  val validationRes = varCheckSub(c, varNames).traverse(x => x)
  validationRes.toEither.flatMap(l => Right(Map[Int,FlowAssertion[_]](l : _*), varNames))
}

def varCheckSub(s : Component, names : VarNames) : List[ValidationResult[(Int,FlowAssertion[_])]] = {
  def subCompsIn(c : Component) : FlowVarSet = {
    c.subcomps.map{sc => sc.fvar.in}.foldLeft(FlowVarSet())(_ ++ _)
  }
  val toBeDef = subCompsIn(s) ++ s.fvar.out
  val defs : Map[Int,ArrayBuffer[FlowAssertion[_]]] = 
    s.assert.assertions groupBy {fv => names.idConverter.flowIntIds(fv.l.uid)}

  val undefined = for {
    v <- toBeDef
    if defs.get(names.idConverter.flowIntIds(v.uid)).isEmpty
  } yield NotDefined(names.getFlow(v.uid), v.line).invalidNec
  val defsList = defs.toList.map{case (id, defSet) => 
    if defSet.size > 1
      val lines = defSet map {_.line}
      MultipleDefinitions(names.getFlow(defSet.head.l.uid), lines.toList).invalidNec
    else
      (id, defSet.head).validNec
  }
  val defsAndUndefs : List[ValidationResult[(Int,FlowAssertion[_])]] = (undefined.toList ++ defsList)
  (s.subcomps.toIterable map (sub => varCheckSub(sub, names))).foldLeft(defsAndUndefs)(_ ++ _)
}

def addEdges[T](lvar : FlowId, expr : Expr[T], g : Graph[Int,DiEdge], idConverter : IdConverter) : Unit = {
  type UnitFunctor[T] = Unit
  g += idConverter.flowIntIds(lvar)
  val r = new Reduction[UnitFunctor]{
    def rFvar[T](x : Expr.Fvar[T]) : Unit = g += idConverter.flowIntIds(x.uid) ~> idConverter.flowIntIds(lvar)
    def rSvar[T](x : Expr.Svar[T]) : Unit = {}
    def rConst[T](x : Expr.Const[T]) : Unit = {}
    def rEq[T](x : Unit, y : Unit) : Unit = {}
    def rIte[T](c : Unit, t : Unit, e : Unit) : Unit ={}
    def rLt[T : Ord](l : Unit, r : Unit) : Unit = {}
    def rNumBin[T : Numeric](b : NumBinop, l : Unit, r : Unit) : Unit = {}
    def rLogBin[T : Logic](b : LogBinop, l : Unit, r : Unit) : Unit = {}
    def rUn[T : Logic](u : Unop, e : Unit) : Unit = {}
  }
  cata(expr, r)
}

def addEdgesComp(c : Component, g : Graph[Int,DiEdge], idConverter : IdConverter) : Unit = {
  for {
    FlowAssertion(l, r, line) <- c.assert.assertions
  } addEdges(l.uid, r, g, idConverter)
  for {
    subcomp <- c.subcomps
  } addEdgesComp(subcomp, g, idConverter)
}

/** Model causality check
  * Builds flow dependency graph
  * Checks each SCC = single node
  * @param c top level component
  * @param vcres result of varCheck
  * @return Flow definitions in topological order for flow propagation
  */
def causalCheck(c : Component, vcres : (Map[Int,FlowAssertion[_]], VarNames)) : EitherResult[FlowDefinition] = {
  val g : Graph[Int,DiEdge] = Graph()
  val (assertByV, varNames) = vcres
  addEdgesComp(c, g, varNames.idConverter)
  val scc = g.strongComponentTraverser().withFilter(_.nodes.size > 1)
  val cyclesLists = scc.map{c => 
    g.findCycleContaining(c.nodes.head)(anyToNode) match
      case Some(c) => c.nodes.toList.map(n => varNames.flowNames(n.toInt)).reverse
      case None => Nil
  }
  val cycles = cyclesLists.map{l => CyclicDefinition(l)}
  val selfDep = assertByV.keys.map{fid => 
    g.find(fid.toInt~>fid.toInt) match
      case Some(_) => fid.toInt
      case None => -1
  }.filter(_>=0).map{i => SelfDependent(varNames.flowNames(i))}
  val causalErrors = cycles ++ selfDep
  causalErrors.headOption match
    case Some(h) => Left(NonEmptyChain.fromChainAppend(Chain.fromSeq(causalErrors.drop(1).toSeq), h))
    case None => 
      g.topologicalSort(anyToNode).fold(
        cycleNode => Left(NonEmptyChain(UnspecifiedError())),
        order => Right(order.toVector map (x => assertByV(x.toInt)))
      )
}

/** Distribution specification checks
  * @param m CausalModel to check
  * @return m or unspecified distribution errors
  */
def distributionsCheck(m : CausalModel) : EitherResult[StochasticModel] = {
  val unspecDistrib = for {
    t <- m.allTransitions.toList
    if t.ev.distrib.isEmpty
  } yield UnspecifiedDistribution(m.varNames.getEvent(t.ev.id)) 
  unspecDistrib match 
    case Nil => 
      Right(StochasticModel(
        m.c,
        m.flowDef,
        m.allNonUrgentTransitions,
        m.allUrgentTransitions,
        m.allStateVars,
        m.allFlowVars,
        m.allTransitions,
        m.idConverter,
        m.varNames)
      )
    case t::q => Left(NonEmptyChain.fromChainAppend(Chain.fromSeq(q.toSeq), t))
}

trait Flattening {
  
  /** Model flattening + flow definition and causality checks
    * @param c top level component
    * @return flattened model or errors
    */
  def compCheck(c : Component) : EitherResult[CausalModel] = {
    def flattenAll(v : Vector[InputTransition], idCv : IdConverter) : Vector[Transition] = {
      val newV = v.foldLeft(v){case (vres, InputTransition(e, _)) => flattenSynchro(vres, idCv.evIntIds(e.id), idCv)}
      newV.map{case InputTransition(e, Synchronizable.Explicit(g, a)::Nil) => Transition(e, g, a)}
    }
    try {
      val stateIntIds = HashMap[StateId,Int]()
      val (svSet, svNum) = allStateVars(c, 0, stateIntIds)
      val allStates = svSet.toVector.sortBy(sv => stateIntIds(sv.uid))

      val flowIntIds = HashMap[FlowId,Int]()
      val (fvSet, fvNum) = allFlowVars(c, 0, flowIntIds)
      val allFlows = fvSet.toVector.sortBy(fv => flowIntIds(fv.uid))

      val evIntIds = HashMap[EventId,Int]()
      val evNum = eventNumbering(c, 0, evIntIds)

      val idConverter = IdConverter(
        evIntIds.toMap, 
        stateIntIds.toMap,
        flowIntIds.toMap,
        evNum, 
        svNum,
        fvNum
      )
      
      for {
        varCheckRes <- varCheck(c, idConverter)
        causalCheckRes <- causalCheck(c, varCheckRes)
        allTrans = allTransitions(c, Vector.fill(evNum)(null), evIntIds)
        flattenedTrans = flattenAll(allTrans, idConverter).sortBy(t => evIntIds(t.ev.id))
        (allUrg, allNonUrg) = flattenedTrans.toList.partition(_.ev.urgent)
        varNames = varCheckRes._2
      } yield CausalModel(
        c, 
        causalCheckRes, 
        allNonUrg, 
        allUrg, 
        allStates, 
        allFlows, 
        flattenedTrans, 
        idConverter, 
        varNames)
    
    } catch{
      case UUIDconflict() => 
        Left(NonEmptyChain.fromNonEmptyList(NonEmptyList(UUIDconflict(), Nil)))
    }
  }

  /** Model flattening + flow definition and causality checks + distribution specification checks
    * @param c top level component
    * @return flattened model or errors
    */
  def stochasticCheck(c : Component) : EitherResult[StochasticModel] = {
    compCheck(c).flatMap{cm => 
      distributionsCheck(cm)
    }
  }
}
