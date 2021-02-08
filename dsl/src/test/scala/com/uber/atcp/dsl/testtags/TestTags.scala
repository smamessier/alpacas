package com.uber.atcp.dsl.testtags

import org.scalatest.Tag


object PropertyBased extends Tag("property based test")

object EquivalenceCheck extends Tag("equivalence checking test")

object Slow extends Tag("a test that takes a lot of time to run")
