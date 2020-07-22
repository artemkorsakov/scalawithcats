package com.github.artemkorsakov.monads

trait MyMonad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(func.andThen(pure))

  // pure(a).flatMap(func) == func(a)
  def leftIdentity[A, B](a: A)(f: A => F[B]): Boolean =
    flatMap(pure(a))(f) == f(a)

  // m.flatMap(pure) == m
  def rightIdentity[A, B](m: F[A]): Boolean =
    flatMap(m)(pure) == m

  // m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
  def associativity[A, B, C](m: F[A])(f: A => F[B])(g: B => F[C]): Boolean =
    flatMap(flatMap(m)(f))(g) == flatMap(m)(x => flatMap(f(x))(g))
}

object MyMonad {
  def apply[F[_]](implicit monad: MyMonad[F]): MyMonad[F] =
    monad
}
