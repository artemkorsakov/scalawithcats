package com.github.artemkorsakov.functors

// fa.map(a => a) == fa
// fa.map(g(f(_))) == fa.map(f).map(g)
trait MyFunctor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
