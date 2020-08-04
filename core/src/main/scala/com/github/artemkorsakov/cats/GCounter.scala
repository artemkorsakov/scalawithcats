package com.github.artemkorsakov.cats

import cats.instances.list._
import cats.instances.map._
import cats.kernel.CommutativeMonoid
import cats.syntax.foldable._
import cats.syntax.semigroup._

final case class GCounter[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)(implicit m: CommutativeMonoid[A]): GCounter[A] = {
    val value = amount |+| counters.getOrElse(machine, m.empty)
    GCounter(counters + (machine -> value))
  }

  def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
    GCounter(this.counters |+| that.counters)

  def total(implicit m: CommutativeMonoid[A]): A =
    this.counters.values.toList.combineAll
}
