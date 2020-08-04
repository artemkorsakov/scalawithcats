package com.github.artemkorsakov.cats

import cats.instances.list._
import cats.instances.map._
import cats.kernel.CommutativeMonoid
import cats.syntax.foldable._
import cats.syntax.semigroup._

final case class GCounterOld[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)(implicit m: CommutativeMonoid[A]): GCounterOld[A] = {
    val value = amount |+| counters.getOrElse(machine, m.empty)
    GCounterOld(counters + (machine -> value))
  }

  def merge(that: GCounterOld[A])(implicit b: BoundedSemiLattice[A]): GCounterOld[A] =
    GCounterOld(this.counters |+| that.counters)

  def total(implicit m: CommutativeMonoid[A]): A =
    this.counters.values.toList.combineAll
}
