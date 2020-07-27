package com.github.artemkorsakov.cats

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monad._

import scala.annotation.tailrec

object OptionMonad {
  val optionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](opt: Option[A])(fn: A => Option[B]): Option[B] =
      opt flatMap fn

    def pure[A](opt: A): Option[A] =
      Some(opt)

    @tailrec
    def tailRecM[A, B](a: A)(fn: A => Option[Either[A, B]]): Option[B] =
      fn(a) match {
        case None           => None
        case Some(Left(a1)) => tailRecM(a1)(fn)
        case Some(Right(b)) => Some(b)
      }
  }

  def retry[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
    f(start).flatMap(a => retry(a)(f))

  def retryM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
    start.iterateWhileM(f)(a => true)

  def retryTailRecM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
    Monad[F].tailRecM(start)(a => f(a).map(a2 => Left(a2)))
}
