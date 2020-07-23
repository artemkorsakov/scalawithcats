package com.github.artemkorsakov.monsemi

trait MySemigroup[A] {
  def combine(x: A, y: A): A
}

trait MyMonoid[A] extends MySemigroup[A] {
  def empty: A
}

object MyMonoid {
  def apply[A](implicit mon: MyMonoid[A]): MyMonoid[A] = mon

  def associativeLaw[A](x: A, y: A, z: A)(implicit mon: MyMonoid[A]): Boolean =
    mon.combine(x, mon.combine(y, z)) == mon.combine(mon.combine(x, y), z)

  def identityLaw[A](x: A)(implicit mon: MyMonoid[A]): Boolean =
    (mon.combine(x, mon.empty) == x) && (mon.combine(mon.empty, x) == x)

}
