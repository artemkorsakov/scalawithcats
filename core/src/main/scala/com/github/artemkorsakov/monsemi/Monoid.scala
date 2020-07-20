package com.github.artemkorsakov.monsemi

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] =
    monoid

  def associativeLaw[A](x: A, y: A, z: A)(
    implicit monoid: Monoid[A]
  ): Boolean = {
    monoid.combine(x, monoid.combine(y, z)) ==
      monoid.combine(monoid.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit monoid: Monoid[A]): Boolean = {
    (monoid.combine(x, monoid.empty) == x) &&
    (monoid.combine(monoid.empty, x) == x)
  }

}
