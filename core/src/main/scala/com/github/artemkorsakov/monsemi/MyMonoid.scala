package com.github.artemkorsakov.monsemi

trait MySemigroup[A] {
  def combine(x: A, y: A): A
}

trait MyMonoid[A] extends MySemigroup[A] {
  def empty: A
}

object MyMonoid {
  def apply[A](implicit monoid: MyMonoid[A]): MyMonoid[A] =
    monoid

  def associativeLaw[A](x: A, y: A, z: A)(
    implicit monoid: MyMonoid[A]
  ): Boolean = {
    monoid.combine(x, monoid.combine(y, z)) ==
      monoid.combine(monoid.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit monoid: MyMonoid[A]): Boolean = {
    (monoid.combine(x, monoid.empty) == x) &&
    (monoid.combine(monoid.empty, x) == x)
  }

}
