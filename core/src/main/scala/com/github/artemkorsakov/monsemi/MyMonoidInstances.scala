package com.github.artemkorsakov.monsemi

object MyMonoidInstances {
  implicit val booleanMonoid: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def empty: Boolean                           = true
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

  implicit def setUnionMonoid[A]: MyMonoid[Set[A]] =
    new MyMonoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] = a union b
      def empty                                 = Set.empty[A]
    }
}
