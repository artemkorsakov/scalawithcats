package com.github.artemkorsakov.cats

import cats.data.{ EitherT, OptionT }

import scala.concurrent.Future

object Transformative {
  type ListOption[A]    = OptionT[List, A]
  type ErrorOr[A]       = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  sealed abstract class HttpError
  final case class NotFound(item: String)  extends HttpError
  final case class BadRequest(msg: String) extends HttpError

  type FutureEither[A]       = EitherT[Future, HttpError, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]
}
