package com.github.artemkorsakov.monsemi

import cats.Monoid
import cats.syntax.semigroup._

case class Order(totalCost: Double, quantity: Double)

object SuperAdder {
  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  def addAll[A](values: List[A])(implicit monoid: Monoid[A]): A =
    values.foldRight(monoid.empty)(_ |+| _)
}
