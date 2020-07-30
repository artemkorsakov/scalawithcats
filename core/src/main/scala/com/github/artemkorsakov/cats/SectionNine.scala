package com.github.artemkorsakov.cats

import cats.Monoid
import cats.instances.future._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.syntax.traverse._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future // for |+|

object SectionNine {

  /** Single-threaded map-reduce function.
    * Maps `func` over `values` and reduces using a `Monoid[B]`.
    */
  def foldMap[A, B: Monoid](values: Vector[A])(func: A => B): B =
    values.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

  def parallelFoldMapOld[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] =
    Future(foldMap(values.grouped(10).toVector.map(v => foldMap(v)(func)))(b => b))

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    // Calculate the number of items to pass to each CPU:
    val numCores  = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.foldMap(func)))
      .map(_.combineAll)
  }

}
