package tutorialpacas.runningexample
import net.sf.javabdd.BDD
import net.sf.javabdd.BDDFactory
import scalaz.Memo
import scala.language.implicitConversions

import com.uber.atcp.dsl._
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.flattening._
import com.uber.atcp.dsl.causalModel._
import com.uber.atcp.dsl.assertion._
import com.uber.atcp.dsl.frontend._
import com.uber.atcp.dsl.ast._
import com.uber.atcp.dsl.errors._
import com.uber.atcp.dsl.stateImpl._

/** Unreliability computation by BDD technique from MCS
  * @param m CausalModel to analyze
  * @param time date of the analysis
  * @param mcs Minimal Cut Sets from minimalCutSets function
  * @return unreliability or error if unspecified distribution
  */
def structureFctBDD(m : CausalModel, mcs : List[MinimalCutSet]) : BDD = {
  val b = BDDFactory.init(1000, 1000)
  b.setVarNum(m.allTransitions.size)
  val conjunctions = mcs.map(_.events.map(ev => b.ithVar(m.idConverter.evIntIds(ev.id))).reduce{_.and(_)})
  conjunctions.reduce{_.or(_)}
}

def unreliabilityBDD(m: CausalModel, bdd: BDD, time: Double, mcs: List[MinimalCutSet]): Double = {
  val probas = Array.tabulate(m.allTransitions.size){i =>  
  val distrib = m.allTransitions(i).ev.distrib
    distrib match
      case Some(InternalDistribution.Dirac(d)) => if d <= time then 1.0 else 0.0
      case Some(InternalDistribution.Exponential(sampler)) => sampler.cumulativeProbability(time)
      case None => throw UnspecifiedDistribution(m.varNames.getEvent(m.allTransitions(i).ev.id))
  }
  lazy val reliability : BDD => Double = Memo.mutableHashMapMemo { bdd =>
    if bdd.isZero() 
      0.0
    else if bdd.isOne()
      1.0
    else 
      val l = bdd.low()
      val r = bdd.high()
      val v = bdd.`var`()
      probas(v) * reliability(r) + (1 - probas(v)) * reliability(l)
  }

  reliability(bdd)
}
