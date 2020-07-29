package com.github.artemkorsakov.cats

import cats.data._

import scala.concurrent.Future

object Transformative {
  type ListOption[A]    = OptionT[List, A]
  type ErrorOr[A]       = Either[String, A]
  type ErrorOrV[A]      = Either[Vector[String], A]
  type ErrorOrList[A]   = Either[List[String], A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  sealed abstract class HttpError
  final case class NotFound(item: String)  extends HttpError
  final case class BadRequest(msg: String) extends HttpError

  type FutureEither[A]       = EitherT[Future, HttpError, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]
}
