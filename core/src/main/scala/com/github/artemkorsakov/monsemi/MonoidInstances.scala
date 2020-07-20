package com.github.artemkorsakov.monsemi

object MonoidInstances {
  implicit val booleanMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def empty: Boolean = true
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] = a union b
      def empty = Set.empty[A]
    }
}
