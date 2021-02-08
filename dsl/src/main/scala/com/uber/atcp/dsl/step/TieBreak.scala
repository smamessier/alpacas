package com.uber.atcp.dsl.step

import scala.language.implicitConversions
import cats.Traverse
import cats.data.Validated._
import cats.implicits._

import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.transition.EventIds._
import com.uber.atcp.dsl.flattening._
import com.uber.atcp.dsl.errors._
import com.uber.atcp.dsl.CausalModel

def tieBreak(m : CausalModel, fireVect : Vector[EventId], rd : Double) : EitherResult[EventId] = {
  /* check all transitions have weights */
  val weights = fireVect.map{evid => 
    m.allTransitions(m.idConverter.evIntIds(evid)).ev.weight match
      case Some(e) => e.validNec
      case None => UnspecifiedExpectation(m.varNames.getEvent(evid)).invalidNec
  }.toList.traverse(x => x).toEither
  /* normalize weights */
  weights.flatMap{exp =>
    val partialExpectations = exp.scanLeft(0.0)(_+_)
    val normalization = partialExpectations.last
    val normalizedPartialExp = partialExpectations.map(_/normalization)
    Right(fireVect(normalizedPartialExp.indexWhere(_ > rd) - 1))
  }
}