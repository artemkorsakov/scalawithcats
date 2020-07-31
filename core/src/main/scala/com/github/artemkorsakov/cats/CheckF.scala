package com.github.artemkorsakov.cats

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.semigroup._

final case class CheckF[E, A](func: A => Either[E, A]) {
  def apply(a: A): Either[E, A] =
    func(a)

  def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
    CheckF { a =>
      (this(a), that(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (Left(e), Right(_))  => e.asLeft
        case (Right(_), Left(e))  => e.asLeft
        case (Right(_), Right(_)) => a.asRight
      }
    }

}

sealed trait CheckS[E, A] {
  import CheckS._

  def and(that: CheckS[E, A]): CheckS[E, A] =
    And(this, that)

  def or(that: CheckS[E, A]): CheckS[E, A] =
    Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(func) =>
        func(a)

      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)

      case Or(left, right) =>
        left(a) match {
          case Valid(a) => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a)    => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }
}

object CheckS {
  final case class And[E, A](left: CheckS[E, A], right: CheckS[E, A]) extends CheckS[E, A]

  final case class Or[E, A](left: CheckS[E, A], right: CheckS[E, A]) extends CheckS[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends CheckS[E, A]
}
