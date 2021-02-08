package com.uber.atcp.dsl.errors

import cats.data.Validated._
import cats.data._

trait AlpacasError extends Error

type ValidationResult[A] = ValidatedNec[AlpacasError,A]
type EitherResult[A] = Either[NonEmptyChain[AlpacasError], A]
